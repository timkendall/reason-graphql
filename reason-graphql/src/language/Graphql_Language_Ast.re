type primitiveValue = [
  | `Int(int)
  | `Float(float)
  | `Boolean(bool)
  | `String(string)
  | `Enum(string)
  | `Null
];

type constValue = [
  primitiveValue
  | `List(list(constValue))
  | `Map(list((string, constValue)))
];

type value = [
  primitiveValue
  | `List(list(value))
  | `Map(list((string, value)))
  | `Variable(string)
];

type document = {definitions: list(definition)}

and definition =
  | TypeSystemDefinition(typeSystemDefinition)
  | OperationDefinition(operationDefinition)
  | FragmentDefinition(fragmentDefinition)

and operationDefinition = {
  operationType,
  name: option(string),
  variableDefinition: list(variableDefinition),
  directives: list(directive),
  selectionSet: list(selection),
}

and operationType =
  | Query
  | Mutation
  | Subscription

and variableDefinition = {
  variable: value,
  typ: typeReference,
  defaultValue: option(constValue),
  directives: list(directive),
}

and selection =
  | Field(field)
  | FragmentSpread(fragmentSpread)
  | InlineFragment(inlineFragmentDefinition)

and field = {
  alias: option(string),
  name: string,
  arguments: list((string, value)),
  selectionSet: list(selection),
  directives: list(directive),
}

and fragmentDefinition = {
  name: string,
  typeCondition: string,
  selectionSet: list(selection),
  directives: list(directive),
}

and inlineFragmentDefinition = {
  typeCondition: option(string),
  selectionSet: list(selection),
  directives: list(directive),
}

and fragmentSpread = {
  name: string,
  directives: list(directive),
}

/* Directives */
and directive = {
  name: string,
  arguments: list((string, value)),
}

and typeReference =
  | NamedType(string)
  | ListType(typeReference)
  | NonNullType(typeReference)

and typeSystemDefinition =
  | SchemaDefinition(schemaDefinition)
  | TypeDefinition(typeDefinition)
  | TypeExtension(typeExtensionDefinition)
  | DirectiveDefinitionNode(directiveDefinition)

and schemaDefinition = {operationTypes: operationTypeDefinition}

and operationTypeDefinition = {
  typ: string,
  operation: operationType,
}

and typeDefinition =
  | ScalarTypeDefinition(string)
  | ObjectTypeDefinition(objectTypeDefinition)
  | InterfaceTypeDefinition(interfaceTypeDefinition)
  | UnionTypeDefinition(unionTypeDefinition)
  | EnumTypeDefinition(enumTypeDefintion)
  | InputObjectTypeDefinition(inputObjectTypeDefinition)

and objectTypeDefinition = {
  name: string,
  interfaces: list(string),
  fields: list(fieldDefinition),
}

and fieldDefinition = {
  name: string,
  arguments: list(inputValueDefinition),
  typ: typeReference,
}

and inputValueDefinition = {
  name: string,
  typ: typeReference,
  defaultValue: option(constValue),
}

and interfaceTypeDefinition = {
  name: string,
  fields: list(fieldDefinition),
}

and unionTypeDefinition = {
  name: string,
  types: list(string),
}

and enumTypeDefintion = {
  name: string,
  values: list(string),
}

and inputObjectTypeDefinition = {
  name: string,
  fields: list(inputValueDefinition),
}

and typeExtensionDefinition = {definition: objectTypeDefinition}

and directiveDefinition = {
  name: string,
  arguments: list(inputValueDefinition),
};
