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
  | TypeSystemExtension(typeSystemExtension)

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
  | DirectiveDefinitionNode(directiveDefinition)

and typeSystemExtension =
  | SchemaExtension(schemaExtension)
  | ScalarTypeExtension(scalarTypeExtension)
  | ObjectTypeExtension(objectTypeExtension)
  | InterfaceTypeExtension(interfaceTypeExtension)
  | UnionTypeExtension(unionTypeExtension)
  | EnumTypeExtension(enumTypeExtension)
  | InputObjectTypeExtension(inputObjectTypeExtension)

and schemaExtension = {
  operationTypes: option(list(operationTypeDefinition)),
  directives: option(list(directive)),
}

and scalarTypeExtension = {
  name: string,
  directives: option(list(directive)),
}

and objectTypeExtension = {
  name: string,
  interfaces: option(list(string)),
  directives: option(list(directive)),
  fields: option(list(fieldDefinition)),
}

and interfaceTypeExtension = {
  name: string,
  directives: option(list(directive)),
  fields: option(list(fieldDefinition)),
}

and unionTypeExtension = {
  name: string,
  types: option(list(string)),
  directives: option(list(directive)),
}

and enumTypeExtension = {
  name: string,
  directives: option(list(directive)),
  values: option(list(string)),
}

and inputObjectTypeExtension = {
  name: string,
  directives: option(list(directive)),
  fields: option(list(inputValueDefinition)),
}

and schemaDefinition = {
  operationTypes: list(operationTypeDefinition),
  directives: list(directive),
}

and operationTypeDefinition = {
  typ: string,
  operation: operationType,
}

and typeDefinition =
  | ScalarTypeDefinition(string)
  | ObjectTypeDefinition(objectTypeDefinition)
  | InterfaceTypeDefinition(interfaceTypeDefinition)
  | UnionTypeDefinition(unionTypeDefinition)
  | EnumTypeDefinition(enumTypeDefinition)
  | InputObjectTypeDefinition(inputObjectTypeDefinition)

and objectTypeDefinition = {
  name: string,
  interfaces: list(string),
  directives: list(directive),
  fields: list(fieldDefinition),
}

and fieldDefinition = {
  description: option(string),
  name: string,
  arguments: list(inputValueDefinition),
  directives: list(directive),
  typ: typeReference,
}

and inputValueDefinition = {
  name: string,
  typ: typeReference,
  directives: list(directive),
  defaultValue: option(value),
}

and interfaceTypeDefinition = {
  name: string,
  directives: list(directive),
  fields: list(fieldDefinition),
}

and unionTypeDefinition = {
  name: string,
  types: list(string),
  directives: list(directive),
}

and enumTypeDefinition = {
  name: string,
  directives: list(directive),
  values: list(string),
}

and inputObjectTypeDefinition = {
  name: string,
  directives: list(directive),
  fields: list(inputValueDefinition),
}

and typeExtensionDefinition = {definition: objectTypeDefinition}

and directiveDefinition = {
  name: string,
  arguments: list(inputValueDefinition),
  repeatable: bool,
  locations: list(directiveLocation),
}

and directiveLocation = 
  | QUERY
  | MUTATION
  | SUBSCRIPTION
  | FIELD
  | FRAGMENT_DEFINITION
  | FRAGMENT_SPREAD
  | INLINE_FRAGMENT
  | SCHEMA
  | SCALAR
  | OBJECT
  | FIELD_DEFINITION
  | ARGUMENT_DEFINITION
  | INTERFACE
  | UNION
  | ENUM
  | ENUM_VALUE
  | INPUT_OBJECT
  | INPUT_FIELD_DEFINITION;