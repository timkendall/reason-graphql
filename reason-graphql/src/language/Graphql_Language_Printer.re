open Graphql_Language_Ast;

let join = (list, seperator) => Belt.List.keep(list, x => x !== "") |> String.concat(seperator);

let wrap = (left, str, right) => str === "" ? "" : left ++ str ++ right;

let indent =
  fun
  | "" => ""
  | str => "  " ++ (str |> Js.String.replaceByRe([%bs.re "/\\n/g"], "\n  "));

let block =
  fun
  | [] => ""
  | list => "{\n" ++ indent(join(list, "\n")) ++ "\n}";

let rec printValue: value => string =
  fun
  | `Int(int) => Js.Int.toString(int)
  | `Float(float) => Js.Float.toString(float)
  | `String(string) => string->Js.Json.string->Js.Json.stringify
  | `Boolean(bool) => string_of_bool(bool)
  | `Null => "null"
  | `Variable(string) => "$" ++ string
  | `Enum(enum) => enum
  | `List(values) => "[" ++ join(values |> Belt.List.map(_, printValue), ", ") ++ "]"
  | `Map(fields) => "{" ++ printObjectFields(fields) ++ "}"

and printObjectFields = fields => fields |> Belt.List.map(_, printObjectField) |> join(_, ", ")

and printObjectField = ((k, v)) => k ++ ": " ++ printValue(v);

let rec printType =
  fun
  | NamedType(string) => string
  | ListType(typ) => "[" ++ printType(typ) ++ "]"
  | NonNullType(typ) => printType(typ) ++ "!";

let printVariableDef = ({variable, typ}: variableDefinition) =>
  printValue(variable) ++ ": " ++ printType(typ);
let printVariables = vars => vars->Belt.List.map(printVariableDef)->join(", ");

let printArgument = ((name, value)) => name ++ ": " ++ printValue(value);
let printArguments = (args: list((string, value))) =>
  args->Belt.List.map(printArgument)->join(", ");

let printDirective = ({name, arguments}: directive) =>
  "@" ++ name ++ wrap("(", printArguments(arguments), ")");

let printDirectives = directives => directives->Belt.List.map(printDirective)->join(" ");

let printOpt =
  fun
  | Some(v) => v
  | None => "";

let rec printSelectionSet = selectionSet =>
  selectionSet |> Belt.List.map(_, printSelection) |> block

and printSelection =
  fun
  | Field(field) => printField(field)
  | FragmentSpread(fragmentSpread) => printFragmentSpread(fragmentSpread)
  | InlineFragment(inlineFragmentDefinition) =>
    printInlineFragmentDefinition(inlineFragmentDefinition)

and printField = ({alias, name, arguments, directives, selectionSet}) =>
  join(
    [
      printAlias(alias) ++ name ++ wrap("(", printArguments(arguments), ")"),
      printDirectives(directives),
      printSelectionSet(selectionSet),
    ],
    " ",
  )

and printAlias =
  fun
  | Some(alias) => alias ++ ": "
  | None => ""

and printFragmentSpread = ({name, directives}) =>
  "..." ++ name ++ wrap(" ", printDirectives(directives), "")

and printInlineFragmentDefinition = ({typeCondition, selectionSet, directives}) =>
  join(
    [
      "...",
      switch (typeCondition) {
      | Some(condition) => wrap("on ", condition, "")
      | None => ""
      },
      printDirectives(directives),
      printSelectionSet(selectionSet),
    ],
    " ",
  );

let printOperationDef =
    ({operationType, variableDefinition, directives, selectionSet} as operationDef) => {
  let operationTypeStr =
    switch (operationType) {
    | Query => "query"
    | Subscription => "subscription"
    | Mutation => "mutation"
    };
  let varDefs = "(" ++ printVariables(variableDefinition) ++ ")";
  let directives = printDirectives(directives);
  let selectionSet = printSelectionSet(selectionSet);

  switch (operationDef) {
  | {operationType: Query, name: None, directives: [], variableDefinition: []} => selectionSet
  | {name} =>
    join(
      [operationTypeStr, join([printOpt(name), varDefs], ""), directives, selectionSet],
      " ",
    )
  };
};

let printFieldDefinition = ({name, arguments, directives, typ}) =>
  join(
    [
      /*name ++ " " ++ wrap("(", printArguments(arguments), ")"),*/
      name,
      ": ",
      printType(typ),
      " " ++ printDirectives(directives),
    ],
    "",
  );


let printFieldsDefinition = fieldsDef =>
  fieldsDef |> Belt.List.map(_, printFieldDefinition) |> block;

let printEnumValueDefinition = (value) =>
  join(
    [
      value
    ],
    "",
  );


let printEnumValuesDefinition = values =>
  values |> Belt.List.map(_, printEnumValueDefinition) |> block;


let printSchemaDef = ({ operationTypes, directives }) => {
  "";
};

let printDirectiveLocation = (location): string => {
  switch(location) {
    | QUERY => "QUERY"
    | MUTATION => "MUTATION"
    | SUBSCRIPTION => "SUBSCRIPTION"
    | FIELD => "FIELD"
    | FRAGMENT_DEFINITION => "FRAGMENT_DEFINITION"
    | FRAGMENT_SPREAD => "FRAGMENT_SPREAD"
    | INLINE_FRAGMENT => "INLINE_FRAGMENT"
    | SCHEMA => "SCHEMA"
    | SCALAR => "SCALAR"
    | OBJECT => "OBJECT"
    | FIELD_DEFINITION => "FIELD_DEFINITION"
    | ARGUMENT_DEFINITION => "ARGUMENT_DEFINITION"
    | INTERFACE => "INTERFACE"
    | UNION => "UNION"
    | ENUM => "ENUM"
    | ENUM_VALUE => "ENUM_VALUE"
    | INPUT_OBJECT => "INPUT_OBJECT"
    | INPUT_FIELD_DEFINITION => "INPUT_FIELD_DEFINITION"
  };
};

let printDefaultValue = (defaultValue) => {
  defaultValue 
    -> Belt.Option.map(_, value => " = " ++ printValue(value))
    -> Belt.Option.getWithDefault(_, "");
}

let printDirectiveDef = ({ name, arguments, repeatable, locations }) => {
  let printableArgs = Belt.List.map(arguments, 
    ({ name, typ, defaultValue }) => {
      name ++ ": " ++ printType(typ) ++ printDefaultValue(defaultValue);
  });
  let printableLocations = locations -> Belt.List.map(_, printDirectiveLocation);
  
  join(
    [
      "directive ",
      "@",
      name, 
      wrap("(", join(printableArgs, ","), ")"),
      repeatable ? " repeatable" : "",
      " on ",
      join(printableLocations, " | "),
    ],
    ""
  );
};

let printObjectTypeDef = ({ name, directives, interfaces, fields }) => {
  let directivesString = printDirectives(directives);
  let interfacesString = Belt.List.length(interfaces) > 0 ? "implements " ++ join(interfaces, " & ") : "";
  let fieldDefsString = printFieldsDefinition(fields);

  join([
    "type",
    name,
    interfacesString,
    directivesString,
    fieldDefsString,
  ], " ");
};

let printInterfaceTypeDef = ({ name, fields, directives }: interfaceTypeDefinition) => {
  let directivesString = printDirectives(directives);
  let fieldDefsString = printFieldsDefinition(fields);

  join([
    "interface",
    name,
    directivesString,
    fieldDefsString,
  ], " ");
};

let printEnumTypeDef = ({ name, values, directives }) => {
  let directivesString = printDirectives(directives);
  let valuesString = printEnumValuesDefinition(values);

  join([
    "enum",
    name,
    directivesString,
    valuesString,
  ], " ");
};

let printInputFieldDefinition = ({name, directives, typ, defaultValue }: inputValueDefinition) =>
  join(
    [
      /*name ++ " " ++ wrap("(", printArguments(arguments), ")"),*/
      name,
      ": ",
      printType(typ),
      printDefaultValue(defaultValue),
      " " ++ printDirectives(directives),
    ],
    "",
  );


let printInputFieldsDefinition = (fieldsDef: list(inputValueDefinition)) =>
  fieldsDef |> Belt.List.map(_, printInputFieldDefinition) |> block;

let printInputObjectTypeDef = ({ name, directives, fields }) => {
  let directivesString = printDirectives(directives);
  /* TODO interfaces */
  let fieldDefsString = printInputFieldsDefinition(fields);

  join([
    "input",
    name,
    directivesString,
    fieldDefsString,
  ], " ");
};

let printUnionTypeDef = ({ name, types, directives }) => {
  let directivesString = printDirectives(directives);
  let typesString = join(types, " | ");

  join([
    "union",
    name,
    "=",
    typesString,
    directivesString,
  ], " ");
};

let printTypeDef = (typeDef) => {
  switch(typeDef) {
  | ScalarTypeDefinition(string) => "scalar " ++ string
  | ObjectTypeDefinition(objectTypeDefinition) => printObjectTypeDef(objectTypeDefinition)
  | InterfaceTypeDefinition(interfaceTypeDefinition) => printInterfaceTypeDef(interfaceTypeDefinition)
  | UnionTypeDefinition(unionTypeDefinition) => printUnionTypeDef(unionTypeDefinition)
  | EnumTypeDefinition(enumTypeDefintion) => printEnumTypeDef(enumTypeDefintion)
  | InputObjectTypeDefinition(inputObjectTypeDefinition) => printInputObjectTypeDef(inputObjectTypeDefinition)
  };
};

let printTypeSystemDef = (typeSystemDef) => {
  switch(typeSystemDef) {
  | SchemaDefinition(schemaDefinition) => printSchemaDef(schemaDefinition)
  | TypeDefinition(typeDefinition) => printTypeDef(typeDefinition)
  | TypeExtension(typeExtensionDefinition) => "Not Implemented"
  | DirectiveDefinitionNode(directiveDefinition) => printDirectiveDef(directiveDefinition)
  };
};

let printFragmentDef = ({name, typeCondition, directives, selectionSet}) =>
  "fragment "
  ++ name
  ++ " on "
  ++ typeCondition
  ++ " "
  ++ wrap("", printDirectives(directives), " ")
  ++ printSelectionSet(selectionSet);

let printDefinition = definition =>
  switch (definition) {
  | TypeSystemDefinition(typeSystemDef) => printTypeSystemDef(typeSystemDef)
  | OperationDefinition(operationDef) => printOperationDef(operationDef)
  | FragmentDefinition(fragmentDef) => printFragmentDef(fragmentDef)
  };

let print = ({definitions}: document) =>
  definitions |> Belt.List.map(_, printDefinition) |> join(_, "\n\n");
