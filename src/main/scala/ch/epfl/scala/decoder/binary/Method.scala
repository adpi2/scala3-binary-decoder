package ch.epfl.scala.decoder.binary

trait Method extends Symbol:
  def declaringClass: BinaryClass
  def signedName: SignedName
  def parameters: Seq[Parameter]
  // return None if the class of the return type is not yet loaded
  def returnType: Option[Type]
  def variables: Seq[Variable]
  def isConstructor: Boolean
  def isStatic: Boolean
  def isFinal: Boolean
  def isBridge: Boolean
  def instructions: Seq[Instruction]

  def name: String = signedName.name

  private[decoder] def isExtensionMethod: Boolean = name.endsWith("$extension") && !isStatic && !isBridge
  private[decoder] def isTraitStaticForwarder: Boolean =
    declaringClass.isInterface && isStatic && name.endsWith("$") && !isDeserializeLambda && !isTraitInitializer
  private[decoder] def isTraitInitializer: Boolean = name == "$init$" && isStatic
  private[decoder] def isClassInitializer: Boolean = name == "<init>"
  private[decoder] def isPartialFunctionApplyOrElse: Boolean = declaringClass.isPartialFunction && name == "applyOrElse"
  private[decoder] def isDeserializeLambda: Boolean =
    isStatic &&
      name == "$deserializeLambda$" &&
      parameters.map(_.`type`.name) == Seq("java.lang.invoke.SerializedLambda")
  private[decoder] def isAnonFun: Boolean = name.matches("(.*)\\$anonfun\\$\\d+")
