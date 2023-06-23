import sbtcrossproject.{ CrossProject, CrossType, Platform }

Global / onChangedBuildSource := ReloadOnSourceChanges

inThisBuild(Seq(
	organization	:= "de.djini",
	version			:= "0.244.0",

	scalaVersion	:= "3.2.2",
	scalacOptions	++= Seq(
		"-feature",
		"-deprecation",
		"-unchecked",
		"-Wunused:all",
		"-Xfatal-warnings",
		"-Ykind-projector:underscores",
	),

	versionScheme	:= Some("early-semver"),

	wartremoverErrors ++= Seq(
		Wart.AsInstanceOf,
		Wart.IsInstanceOf,
		Wart.StringPlusAny,
		Wart.ToString,
		Wart.EitherProjectionPartial,
		Wart.OptionPartial,
		Wart.TryPartial,
		Wart.Enumeration,
		Wart.FinalCaseClass,
		Wart.JavaConversions,
		Wart.Option2Iterable,
		Wart.JavaSerializable,
		//Wart.Any,
		Wart.AnyVal,
		//Wart.Nothing,
		Wart.ArrayEquals,
		Wart.ImplicitParameter,
		Wart.ExplicitImplicitTypes,
		Wart.LeakingSealed,
		Wart.DefaultArguments,
		Wart.Overloading,
		//Wart.PublicInference,
		//Wart.TraversableOps,
	),
))

lazy val noTestSettings	=
	Seq(
		test		:= {},
		testQuick	:= {}
	)

// (crossProject crossType CrossType.Pure in base)
def myCrossProject(id:String, base:File, crossType:CrossType):CrossProject	=
	CrossProject(
		id		= id,
		base	= base,
	)(
		JVMPlatform,
		JSPlatform
	)
	.crossType(crossType)
	.settings(
		name := id
	)
	.configurePlatform(JVMPlatform)	(_ withId (id + "-jvm"))
	.configurePlatform(JSPlatform)	(_ withId (id + "-js"))

lazy val `scparse` =
	(project in file("."))
	.aggregate(
		`scparse-ng-jvm`,
		`scparse-ng-js`,
	)
	.settings(
		publishArtifact := false
	)

//------------------------------------------------------------------------------

lazy val `scparse-ng`	=
	myCrossProject("scparse-ng", file("modules/ng"), CrossType.Pure)
	.enablePlugins()
	// TODO this crashes the build with "no such key exception" on JVMPlatform and/or JSPlatform - why?
	//.dependsOn()
	.settings(
		libraryDependencies	++= Seq(
			"de.djini"		%%% "scutil-core"	% "0.235.0"	% "compile",
			"io.monix"		%%	"minitest"		% "2.9.6"	% "test"
		),
		testFrameworks	+= new TestFramework("minitest.runner.Framework")
	)
	.jvmSettings()
	.jsSettings(
		noTestSettings
	)
lazy val `scparse-ng-jvm`	= `scparse-ng`.jvm
lazy val `scparse-ng-js`	= `scparse-ng`.js

