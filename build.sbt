import sbtcrossproject.{ CrossProject, CrossType, Platform }

inThisBuild(Seq(
	organization	:= "de.djini",
	version			:= "0.180.0",

	scalaVersion	:= "2.13.1",
	scalacOptions	++= Seq(
		"-deprecation",
		"-unchecked",
		"-feature",
		"-language:higherKinds",
		"-Werror",
		"-Xlint",
	),

	conflictManager		:= ConflictManager.strict withOrganization "^(?!(org\\.scala-lang|org\\.scala-js)(\\..*)?)$",
	addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),

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
			"de.djini"		%% "scutil-core"	% "0.174.0"	% "compile",
			"org.specs2"	%% "specs2-core"	% "4.8.3"	% "test"
		),
	)

lazy val `scparse-ng`	=
	myCrossProject("scparse-ng", file("modules/ng"), CrossType.Pure)
	.enablePlugins()
	// TODO this crashes the build with "no suck key exception" on JVMPlatform and/or JSPlatform - why?
	//.dependsOn()
	.settings(
		libraryDependencies	++= Seq(
			"de.djini"		%%% "scutil-base"	% "0.174.0"	% "compile",
		),
	)
	.jvmSettings()
	.jsSettings(
		noTestSettings
	)
lazy val `scparse-ng-jvm`	= `scparse-ng`.jvm
lazy val `scparse-ng-js`	= `scparse-ng`.js


