package ch.epfl.scala.decoder.internal

import tastyquery.Contexts.*
import tastyquery.Names.*
import tastyquery.Types.*

object Definitions:
  def scalaRuntimePackage(using Context) = defn.scalaPackage.getPackageDecl(SimpleName("runtime")).get
  def javaPackage(using Context) = defn.RootPackage.getPackageDecl(SimpleName("java")).get
  def javaIoPackage(using Context) = javaPackage.getPackageDecl(SimpleName("io")).get
  def javaLangInvokePackage(using Context) = defn.javaLangPackage.getPackageDecl(SimpleName("invoke")).get

  def PartialFunctionClass(using Context) = defn.scalaPackage.getDecl(typeName("PartialFunction")).get.asClass
  def AbstractPartialFunctionClass(using Context) =
    scalaRuntimePackage.getDecl(typeName("AbstractPartialFunction")).get.asClass
  def SerializableClass(using Context) = javaIoPackage.getDecl(typeName("Serializable")).get.asClass
  def javaLangEnumClass(using Context) = defn.javaLangPackage.getDecl(typeName("Enum")).get.asClass

  def SerializedLambdaType(using Context): Type =
    TypeRef(javaLangInvokePackage.packageRef, typeName("SerializedLambda"))
  def DeserializeLambdaType(using Context) =
    MethodType(List(SimpleName("arg0")), List(SerializedLambdaType), defn.ObjectType)

  def Function0Type(using Context) = TypeRef(defn.scalaPackage.packageRef, typeName("Function0"))
