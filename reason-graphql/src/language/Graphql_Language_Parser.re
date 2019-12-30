open Graphql_Language_Ast;
module Lexer = Graphql_Language_Lexer;

module Result = {
  include Belt.Result;
  let let_ = flatMap;
};

type result('a) = Result.t('a, Graphql_Language_Error.t);

let syntaxError = a => Result.Error(Graphql_Language_Error.SyntaxError(a));

let expectedError = (lexer: Lexer.t, token: Lexer.token) => {
  syntaxError(
    "Expected" ++ Lexer.tokenKind(token) ++ ", found " ++ Lexer.tokenDesc(lexer.curr),
  );
};

let expect = (lexer: Lexer.t, token: Lexer.token) =>
  switch (lexer.curr.token) {
  | currToken when currToken == token => Lexer.advance(lexer)
  | _ => expectedError(lexer, token)
  };
  
let expectOptional = (lexer: Lexer.t, token: Lexer.token) =>
  switch (expect(lexer, token)) {
  | Ok(_) => true
  | Error(_) => false
  };

let skip = (lexer: Lexer.t, skipToken: Lexer.token): result(bool) =>
  switch (lexer.curr.token) {
  | token when token == skipToken => Lexer.advance(lexer)->Result.map(_ => true)
  | _ => Ok(false)
  };

let skipKeyword = (lexer: Lexer.t, value: string): result(bool) =>
  switch (lexer.curr.token) {
  | Name(name) when name == value => Lexer.advance(lexer)->Result.map(_ => true)
  | _ => Ok(false)
  };

let expectKeyword = (lexer: Lexer.t, value: string): result(unit) => {
  let%Result skipped = skipKeyword(lexer, value);
  if (!skipped) {
    syntaxError("Expected " ++ value ++ ", found " ++ Lexer.tokenDesc(lexer.curr));
  } else {
    Ok();
  };
};

let expectOptionalKeyword = (lexer: Lexer.t, value: string): result(bool) => {
  let%Result skipped = skipKeyword(lexer, value);
  Ok(skipped);
};

let unexpected = (lexer: Lexer.t) => {
  syntaxError("Unexpected " ++ Lexer.tokenDesc(lexer.curr));
};

let any =
    (
      lexer: Lexer.t,
      openKind: Lexer.token,
      parseFn: Lexer.t => result('a),
      closeKind: Lexer.token,
    ) => {
  let%Result _ = expect(lexer, openKind);

  let rec collect = nodes => {
    let%Result skipped = skip(lexer, closeKind);
    if (!skipped) {
      let%Result node = parseFn(lexer);
      collect([node, ...nodes]);
    } else {
      Ok(Belt.List.reverse(nodes));
    };
  };

  collect([]);
};

let many =
    (
      lexer: Lexer.t,
      openKind: Lexer.token,
      parseFn: Lexer.t => result('a),
      closeKind: Lexer.token,
    )
    : result(list('a)) => {
  let%Result _ = expect(lexer, openKind);
  let%Result node = parseFn(lexer);

  let rec collect = nodes => {
    let%Result skipped = skip(lexer, closeKind);
    if (!skipped) {
      let%Result node = parseFn(lexer);
      collect([node, ...nodes]);
    } else {
      Ok(Belt.List.reverse(nodes));
    };
  };

  collect([node]);
};

let parseName =
  fun
  | ({curr: {token: Name(value)}} as lexer: Lexer.t) =>
    Lexer.advance(lexer)->Result.map(_ => value)

  | lexer => expectedError(lexer, Name(""));

let parseNamedType = (lexer: Lexer.t) => {
  let%Result name = parseName(lexer);
  Ok(NamedType(name));
};

let parseVariable = (lexer: Lexer.t) => {
  let%Result _ = expect(lexer, Dollar);
  let%Result name = parseName(lexer);
  Ok(`Variable(name));
};

let rec parseValueLiteral = (lexer: Lexer.t, ~isConst: bool): result(value) =>
  Result.(
    switch (lexer.curr.token) {
    | BracketOpen =>
      any(lexer, BracketOpen, parseValueLiteral(~isConst), BracketClose)
      ->map(list => `List(list))
    | BraceOpen => parseObject(lexer, ~isConst)
    | Int(value) => Lexer.advance(lexer)->map(_ => `Int(int_of_string(value)))
    | Float(value) => Lexer.advance(lexer)->map(_ => `Float(float_of_string(value)))
    | String(value) => Lexer.advance(lexer)->map(_ => `String(value))
    | Name("true") => Lexer.advance(lexer)->map(_ => `Boolean(true))
    | Name("false") => Lexer.advance(lexer)->map(_ => `Boolean(false))
    | Name("null") => Lexer.advance(lexer)->map(_ => `Null)
    | Name(value) => Lexer.advance(lexer)->map(_ => `Enum(value))
    | Dollar when !isConst => parseVariable(lexer)
    | _ => unexpected(lexer)
    }
  )

and parseObject = (lexer: Lexer.t, ~isConst: bool) => {
  let%Result _ = expect(lexer, BraceOpen);

  let rec parseFields = fields => {
    let%Result skipped = skip(lexer, BraceClose);
    if (!skipped) {
      let%Result field = parseObjectField(lexer, ~isConst);
      parseFields([field, ...fields]);
    } else {
      Ok(fields);
    };
  };

  let%Result fields = parseFields([]);
  Ok(`Map(Belt.List.reverse(fields)));
}

and parseObjectField = (lexer: Lexer.t, ~isConst: bool): result((string, value)) => {
  let%Result name = parseName(lexer);
  let%Result _ = expect(lexer, Colon);
  let%Result value = parseValueLiteral(lexer, ~isConst);
  Ok((name, value));
};

let rec parseTypeReference = (lexer: Lexer.t) => {
  let%Result typ = {
    let%Result skipped = skip(lexer, BracketOpen);
    if (skipped) {
      let%Result t = parseTypeReference(lexer);
      let%Result _ = expect(lexer, BracketClose);
      Ok(ListType(t));
    } else {
      let%Result typ = parseNamedType(lexer);
      Ok(typ);
    };
  };

  let%Result skipped = skip(lexer, Bang);

  skipped ? Ok(NonNullType(typ)) : Ok(typ);
};

let parseArgument = (lexer: Lexer.t, ~isConst): result((string, value)) => {
  let%Result name = parseName(lexer);
  let%Result _ = expect(lexer, Colon);
  let%Result valueLiteral = parseValueLiteral(lexer, ~isConst);
  Ok((name, valueLiteral));
};

let parseArguments = (lexer: Lexer.t, ~isConst: bool) =>
  switch (lexer.curr.token) {
  | ParenOpen => many(lexer, ParenOpen, parseArgument(~isConst), ParenClose)
  | _ => Ok([])
  };

let parseDirective = (lexer: Lexer.t, ~isConst: bool): result(directive) => {
  let%Result _ = expect(lexer, At);
  let%Result name = parseName(lexer);
  let%Result arguments = parseArguments(lexer, ~isConst);
  Ok({name, arguments}: directive);
};

let parseDirectives = (lexer: Lexer.t, ~isConst: bool) => {
  let rec collect = directives =>
    switch (lexer.curr.token) {
    | At =>
      let%Result directive = parseDirective(lexer, ~isConst);
      collect([directive, ...directives]);
    | _ => Ok(Belt.List.reverse(directives))
    };

  collect([]);
};

/* Operation Definitions */

let parseOperationType = (lexer: Lexer.t): result(Graphql_Language_Ast.operationType) => {
  switch (lexer.curr.token) {
  | Name("query") => Result.Ok(Query)
  | Name("mutation") => Ok(Mutation)
  | Name("subscription") => Ok(Subscription)
  | _ => unexpected(lexer)
  };
};

let parseVariableDefinition = (lexer: Lexer.t): result(variableDefinition) => {
  let%Result variable = parseVariable(lexer);
  let%Result _ = expect(lexer, Colon);
  let%Result typ = parseTypeReference(lexer);
  let%Result directives = parseDirectives(lexer, ~isConst=true);
  Ok({typ, variable, defaultValue: None, directives});
};

let parseVariableDefinitions = (lexer: Lexer.t) =>
  switch (lexer.curr.token) {
  | ParenOpen => many(lexer, ParenOpen, parseVariableDefinition, ParenClose)
  | _ => Ok([])
  };

let rec parseSelectionSet = (lexer: Lexer.t): result(list(selection)) => {
  many(lexer, BraceOpen, parseSelection, BraceClose);
}

and parseSelection = (lexer: Lexer.t): result(selection) =>
  switch (lexer.curr.token) {
  | Spread => parseFragment(lexer)
  | _ => parseField(lexer)
  }

and parseFragmentName = (lexer: Lexer.t) =>
  switch (lexer.curr.token) {
  | Name("on") => unexpected(lexer)
  | _ => parseName(lexer)
  }

and parseFragment = (lexer: Lexer.t) => {
  let%Result _ = expect(lexer, Spread);
  let%Result hasTypeCondition = skipKeyword(lexer, "on");

  switch (lexer.curr.token) {
  | Name(_) when !hasTypeCondition =>
    let%Result name = parseFragmentName(lexer);
    let%Result directives = parseDirectives(lexer, ~isConst=false);
    Ok(FragmentSpread({name, directives}));
  | _ =>
    let typeCondition =
      hasTypeCondition
        ? switch (parseName(lexer)) {
          | Ok(name) => Some(name)
          | _ => None
          }
        : None;
    let%Result directives = parseDirectives(lexer, ~isConst=false);
    let%Result selectionSet = parseSelectionSet(lexer);
    Ok(InlineFragment({typeCondition, directives, selectionSet}));
  };
}

and parseField = (lexer: Lexer.t) => {
  let%Result name = parseName(lexer);

  let%Result (alias, name) = {
    let%Result skipped = skip(lexer, Colon);
    if (skipped) {
      let%Result name2 = parseName(lexer);
      Ok((Some(name), name2));
    } else {
      Ok((None, name));
    };
  };

  let%Result arguments = parseArguments(lexer, ~isConst=false);
  let%Result directives = parseDirectives(lexer, ~isConst=false);
  let%Result selectionSet =
    switch (lexer.curr.token) {
    | BraceOpen => parseSelectionSet(lexer)
    | _ => Ok([])
    };

  Ok(Field({name, alias, arguments, directives, selectionSet}));
};

let parseOperationDefinition = (lexer: Lexer.t) =>
  switch (lexer.curr.token) {
  | BraceOpen =>
    let%Result selectionSet = parseSelectionSet(lexer);
    Ok(
      OperationDefinition({
        operationType: Query,
        name: None,
        variableDefinition: [],
        directives: [],
        selectionSet,
      }),
    );
  | _ =>
    let%Result operationType = parseOperationType(lexer);
    let%Result _ = Lexer.advance(lexer);
    let%Result name =
      switch (lexer.curr.token) {
      | Name(name) => Lexer.advance(lexer)->Result.map(_ => Some(name))
      | _ => Ok(None)
      };

    let%Result variableDefinition = parseVariableDefinitions(lexer);
    let%Result directives = parseDirectives(lexer, ~isConst=false);
    let%Result selectionSet = parseSelectionSet(lexer);

    Ok(OperationDefinition({operationType, name, variableDefinition, directives, selectionSet}));
  };

let parseFragmentDefinition = (lexer: Lexer.t) => {
  let%Result () = expectKeyword(lexer, "fragment");
  let%Result name = parseFragmentName(lexer);
  let%Result () = expectKeyword(lexer, "on");

  let%Result typeCondition = parseName(lexer);
  let%Result selectionSet = parseSelectionSet(lexer);
  let%Result directives = parseDirectives(lexer, ~isConst=false);

  Ok(FragmentDefinition({typeCondition, name, selectionSet, directives}));
};

/* Type System Definitions */

let parseDescription =
  fun
  | ({curr: {token: Desc(value)}} as lexer: Lexer.t) =>
    Lexer.advance(lexer)->Result.map(_ => Some(value))
  | lexer => Ok(None);

let parseArgumentDefinition = (lexer: Lexer.t): result(inputValueDefinition) => {
  let%Result name = parseName(lexer);
  let%Result _ = expect(lexer, Colon);
  let%Result typ = parseTypeReference(lexer);
  let%Result directives = parseDirectives(lexer, ~isConst=true);
 
  let%Result hasDefaultValue = skip(lexer, Equals);
  let%Result defaultValue = {
    if (hasDefaultValue) {
      let%Result value = parseValueLiteral(lexer, ~isConst=true);
      Ok(Some(value));
    } else {
      Ok(None);
    }
  }

  Ok({name, typ, directives, defaultValue });
};

let parseArgumentDefinitions = (lexer: Lexer.t) =>
  switch (lexer.curr.token) {
  | ParenOpen => many(lexer, ParenOpen, parseArgumentDefinition, ParenClose)
  | _ => Ok([])
  };
  

let rec parseFieldsDefinition = (lexer: Lexer.t): result(list(fieldDefinition)) => {
  many(lexer, BraceOpen, parseFieldDefinition, BraceClose);
}

and parseFieldDefinition = (lexer: Lexer.t) => {
  let%Result description = parseDescription(lexer);
  let%Result name = parseName(lexer);
  let%Result arguments = parseArgumentDefinitions(lexer);
  let%Result _ = skip(lexer, Colon);
  let%Result typ = parseTypeReference(lexer);
  let%Result directives = parseDirectives(lexer, ~isConst=false);

  Ok({ name, description, arguments, directives, typ });
};

let parseOperationTypeDefinition = (lexer: Lexer.t): result(operationTypeDefinition) => {
  let%Result operation = parseOperationType(lexer);
  let%Result _ = Lexer.advance(lexer);
  let%Result _ = expect(lexer, Colon);
  let%Result typ = parseName(lexer);

  Ok({ operation, typ });
};

let parseOperationTypeDefinitions = (lexer: Lexer.t): result(list(operationTypeDefinition)) => 
  many(lexer, BraceOpen, parseOperationTypeDefinition, BraceClose);

let parseSchemaDefinition = (lexer: Lexer.t) => {
  let%Result _ = Lexer.advance(lexer);
  let%Result directives = parseDirectives(lexer, ~isConst=true);
  let%Result operationTypes = parseOperationTypeDefinitions(lexer);

  Ok(SchemaDefinition({ operationTypes, directives }));
};

let parseInterfaceTypeDefinition = (lexer: Lexer.t) => {
  let%Result _ = Lexer.advance(lexer);
  let%Result name = parseName(lexer);
  let%Result directives = parseDirectives(lexer, ~isConst=false);
  let%Result fields = parseFieldsDefinition(lexer);

  Ok(InterfaceTypeDefinition({ name, directives, fields }));
};

let rec parseImplementsInterfaces = (lexer: Lexer.t, interfaces: list(string)) => {
  let%Result hasImplementation = expectOptionalKeyword(lexer, "implements");

  if (Belt.List.length(interfaces) > 0 || hasImplementation) {
    /* Optional leading ampersand */
    let _ = expectOptional(lexer, Amp);
    let%Result name = parseName(lexer);

    /* Multiple implementations */
    if (expectOptional(lexer, Amp)) {
      parseImplementsInterfaces(lexer, [ name, ...interfaces ]);
    } else {
      Ok([ name, ...interfaces ]);
    }
  } else {
    Ok(interfaces);
  }
};

let parseObjectTypeDefinition = (lexer: Lexer.t) => {
  let%Result _ = Lexer.advance(lexer);
  let%Result name = parseName(lexer);
  let%Result interfaces = parseImplementsInterfaces(lexer, []) 
    -> Belt.Result.map(_, Belt.List.reverse);
  let%Result directives = parseDirectives(lexer, ~isConst=false);
  let%Result fields = parseFieldsDefinition(lexer);

  Ok(ObjectTypeDefinition({ name, interfaces, directives, fields }));
};


let parseScalarTypeDefinition = (lexer: Lexer.t) => {
  let%Result _ = Lexer.advance(lexer);
  let%Result name = parseName(lexer);
  /* let%Result directives = parseDirectives(lexer, ~isConst=false); */

  Ok(ScalarTypeDefinition(name));
};


let parseInputValueDefinition = (lexer: Lexer.t): result(inputValueDefinition) => {
  let%Result name = parseName(lexer);
  let%Result _ = expect(lexer, Colon);
  let%Result typ = parseTypeReference(lexer);

  let%Result hasDefaultValue = skip(lexer, Equals);
  
  let%Result defaultValue = {
    if (hasDefaultValue) {
      let%Result value = parseValueLiteral(lexer, ~isConst=true);
      Ok(Some(value));
    } else {
      Ok(None);
    }
  }
  let%Result directives = parseDirectives(lexer, ~isConst=true);

  Ok({ name, typ, defaultValue, directives });
};

let parseInputValueDefinitions = (lexer: Lexer.t) =>
  switch (lexer.curr.token) {
  | ParenOpen => many(lexer, ParenOpen, parseInputValueDefinition, ParenClose)
  | _ => Ok([])
  };


let parseInputObjectDefinitions = (lexer: Lexer.t) =>
  switch (lexer.curr.token) {
  | BraceOpen => many(lexer, BraceOpen, parseInputValueDefinition, BraceClose)
  | _ => Ok([])
  };

let parseInputObjectTypeDefinition = (lexer: Lexer.t) => {
  let%Result _ = Lexer.advance(lexer);
  let%Result name = parseName(lexer);  
  let%Result directives = parseDirectives(lexer, ~isConst=false);
  let%Result fields = parseInputObjectDefinitions(lexer);

  Ok(InputObjectTypeDefinition({ name, directives, fields }));
};


let rec parseDirectiveLocations = (lexer: Lexer.t, locations: list(directiveLocation)) => {
  let%Result location = parseDirectiveLocation(lexer);
  let%Result hasNext = skip(lexer, Pipe);

  if (hasNext) {
    parseDirectiveLocations(lexer, [location, ...locations]);
  } else {
    Ok([location, ...locations]);
  };
}

and parseDirectiveLocation = (lexer: Lexer.t) => {
  switch(parseName(lexer)) {
    | Ok("QUERY") => Belt.Result.Ok(QUERY)
    | Ok("MUTATION") => Ok(MUTATION)
    | Ok("SUBSCRIPTION") => Ok(SUBSCRIPTION)
    | Ok("FIELD") => Ok(FIELD)
    | Ok("FRAGMENT_DEFINITION") => Ok(FRAGMENT_DEFINITION)
    | Ok("FRAGMENT_SPREAD") => Ok(FRAGMENT_SPREAD)
    | Ok("INLINE_FRAGMENT") => Ok(INLINE_FRAGMENT)
    | Ok("SCHEMA") => Ok(SCHEMA)
    | Ok("SCALAR") => Ok(SCALAR)
    | Ok("OBJECT") => Ok(OBJECT)
    | Ok("FIELD_DEFINITION") => Ok(FIELD_DEFINITION)
    | Ok("ARGUMENT_DEFINITION") => Ok(ARGUMENT_DEFINITION)
    | Ok("INTERFACE") => Ok(INTERFACE)
    | Ok("UNION") => Ok(UNION)
    | Ok("ENUM") => Ok(ENUM)
    | Ok("ENUM_VALUE") => Ok(ENUM_VALUE)
    | Ok("INPUT_OBJECT") => Ok(INPUT_OBJECT)
    | Ok("INPUT_FIELD_DEFINITION") => Ok(INPUT_FIELD_DEFINITION)
    | _ => unexpected(lexer)
  };
};

let parseDirectiveDefinition = (lexer: Lexer.t) => {
  let%Result _ = Lexer.advance(lexer);
  let%Result _ = expect(lexer, At);
  let%Result name = parseName(lexer);
  let%Result arguments = parseInputValueDefinitions(lexer);
  
  let%Result repeatable = expectOptionalKeyword(lexer, "repeatable");
  let%Result _ = expectKeyword(lexer, "on");
  
  /* Optional leading pipe */
  let _ = expectOptional(lexer, Pipe);
  let%Result locations = parseDirectiveLocations(lexer, []) -> Belt.Result.map(_, Belt.List.reverse);

  Ok(DirectiveDefinitionNode({
    name, 
    repeatable, 
    arguments, 
    locations,
  }));
};

let rec parseEnumValuesDefinition = (lexer: Lexer.t): result(list(string)) => {
  many(lexer, BraceOpen, parseEnumValueDefinition, BraceClose);
}

and parseEnumValueDefinition = (lexer: Lexer.t): result(string) => {
   /* let%Result directives = parseDirectives(lexer, ~isConst=true); */
  let%Result name = parseName(lexer);

  // let%Result _ = expectOptional(lexer, Comma)
  Ok(name);
};

let parseEnumTypeDefinition = (lexer: Lexer.t) => {
  let%Result _ = Lexer.advance(lexer);
  let%Result name = parseName(lexer);
  let%Result directives = parseDirectives(lexer, ~isConst=true);
  let%Result values = parseEnumValuesDefinition(lexer);

  Ok(EnumTypeDefinition({ name, values, directives }))
};


let rec parseUnionMemberTypes = (lexer: Lexer.t, types: list(string)) => {
  let%Result _type = parseName(lexer);
  let%Result hasNext = skip(lexer, Pipe);

  if (hasNext) {
    parseUnionMemberTypes(lexer, [_type, ...types]);
  } else {
    Ok([_type, ...types]);
  };
};

let parseUnionTypeDefinition = (lexer: Lexer.t) => {
  let%Result _ = Lexer.advance(lexer);
  let%Result name = parseName(lexer);
  let%Result _ = expect(lexer, Equals);
  let%Result types = parseUnionMemberTypes(lexer, []);
  let%Result directives = parseDirectives(lexer, ~isConst=true);

  Ok(UnionTypeDefinition({ name, types, directives }));
};

let parseTypeDefinition = (lexer: Lexer.t) => {
  let%Result result = switch (lexer.curr.token) {
  | Name("scalar") => parseScalarTypeDefinition(lexer)
  | Name("type") => parseObjectTypeDefinition(lexer)
  | Name("interface") => parseInterfaceTypeDefinition(lexer)
  | Name("union") => parseUnionTypeDefinition(lexer) 
  | Name("enum") => parseEnumTypeDefinition(lexer)
  | Name("input") => parseInputObjectTypeDefinition(lexer)
  | _ => unexpected(lexer)
  };

  Ok(TypeDefinition(result));
};

let parseTypeSystemDefinition = (lexer: Lexer.t) => {
  let%Result result = switch (lexer.curr.token) {
  | Name("schema") => parseSchemaDefinition(lexer)
  | Name("scalar" | "type" | "interface" | "union" | "enum" | "input") => parseTypeDefinition(lexer)
  | Name("directive") => parseDirectiveDefinition(lexer)
  /* TODO `extend` */
  | _ => unexpected(lexer)
  };
  
  Ok(TypeSystemDefinition(result));
};

/* Type System Extension Definitions */
/* TODO */

/* Entry */

let parseExecutableDefinition = (lexer: Lexer.t) =>
  switch (lexer.curr.token) {
  | Name("query" | "mutation" | "subscription")
  | BraceOpen => parseOperationDefinition(lexer)
  | Name("fragment") => parseFragmentDefinition(lexer)
  | _ => unexpected(lexer)
  };

let parseDefinition = (lexer: Lexer.t) =>
  switch (lexer.curr.token) {
  | Name("query" | "mutation" | "subscription" | "fragment")
  | BraceOpen => parseExecutableDefinition(lexer)
  | Name("schema" | "scalar" | "type" | "interface" | "union" | "enum" | "input" | "directive") => parseTypeSystemDefinition(lexer)
  /*| Name("extend") => parseTypeSystemExtension(lexer)*/
  | _ => unexpected(lexer)
  };

let parseDocument = (lexer: Lexer.t): result(document) => {
  let%Result definitions = many(lexer, StartOfFile, parseDefinition, EndOfFile);
  Ok({definitions: definitions});
};

let parse = (body: string) => {
  let lexer = Lexer.make(body);
  parseDocument(lexer);
};