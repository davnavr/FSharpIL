namespace FSharpIL.Metadata

// II.22.21
[<StructuralComparison; StructuralEquality>]
type GenericParamConstraint =
    | ClassConstraint of TypeIndex<ConcreteClassDef>
    /// <summary>
    /// Indicates that the generic parameter is constrainted to derive from a <c>TypeDef</c> representing an abstract class.
    /// </summary>
    | AbstractClassConstraint of TypeIndex<AbstractClassDef>
    | InterfaceConstraint of TypeIndex<InterfaceDef>
    /// <summary>
    /// Indicates that the generic parameter is constrainted to derive from a <c>TypeRef</c> representing a class or interface.
    /// </summary>
    | TypeRefConstraint of SimpleIndex<TypeRef>
    // | TypeSpecConstraint

    interface IGenericParamConstraint
    interface IIndexValue with
        member this.CheckOwner owner =
            match this with
            | ClassConstraint (SimpleIndex concrete) -> IndexOwner.checkIndex owner concrete
            | AbstractClassConstraint (SimpleIndex abst) -> IndexOwner.checkIndex owner abst
            | InterfaceConstraint (SimpleIndex intf) -> IndexOwner.checkIndex owner intf
            | TypeRefConstraint tref -> IndexOwner.checkIndex owner tref
