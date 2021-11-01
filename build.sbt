import sbtcrossproject.{ CrossProject, CrossType, Platform }

Global / onChangedBuildSource := ReloadOnSourceChanges

inThisBuild(Seq(
	organization	:= "de.djini",
	version			:= "0.217.0",

	scalaVersion	:= "2.13.6",
	scalacOptions	++= Seq(
		"-feature",
		"-deprecation",
		"-unchecked",
		"-Werror",
		"-Xlint",
		"-Xsource:3",
		"-P:kind-projector:underscore-placeholders",
	),

	versionScheme	:= Some("early-semver"),
	conflictManager	:= ConflictManager.strict withOrganization "^(?!(org\\.scala-lang|org\\.scala-js)(\\..*)?)$",

	addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full),

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
		Wart.TraversableOps,
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
		`scparse-oldschool`,
		`scparse-ng-jvm`,
		`scparse-ng-js`,
	)
	.settings(
		publishArtifact := false
	)

//------------------------------------------------------------------------------

lazy val `scparse-oldschool`	=
	(project in file("modules/oldschool"))
	.enablePlugins()
	.dependsOn()
	.settings(
		libraryDependencies	++= Seq(
			"de.djini"		%%	"scutil-jdk"	% "0.210.1"	% "compile",
			"io.monix"		%%	"minitest"		% "2.9.6"	% "test"
		),
		testFrameworks	+= new TestFramework("minitest.runner.Framework")
	)

lazy val `scparse-ng`	=
	myCrossProject("scparse-ng", file("modules/ng"), CrossType.Pure)
	.enablePlugins()
	// TODO this crashes the build with "no such key exception" on JVMPlatform and/or JSPlatform - why?
	//.dependsOn()
	.settings(
		libraryDependencies	++= Seq(
			"de.djini"		%%% "scutil-core"	% "0.210.1"	% "compile",
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

