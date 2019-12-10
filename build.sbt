inThisBuild(Seq(
	organization	:= "de.djini",
	version			:= "0.167.0",

	scalaVersion	:= "2.13.1",
	scalacOptions	++= Seq(
		"-deprecation",
		"-unchecked",
		// "-language:implicitConversions",
		// "-language:existentials",
		"-language:higherKinds",
		// "-language:reflectiveCalls",
		// "-language:dynamics",
		// "-language:experimental.macros"
		"-feature",
		"-Xfatal-warnings",
		"-Xlint"
	),

	conflictManager		:= ConflictManager.strict withOrganization "^(?!(org\\.scala-lang|org\\.scala-js)(\\..*)?)$",

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

lazy val `scparse` =
	(project in file("."))
	.aggregate(
		`scparse-oldschool`,
		`scparse-ng`,
	)
	.settings(
		publishArtifact := false
		//publish		:= {},
		//publishLocal	:= {}
	)

//------------------------------------------------------------------------------

lazy val `scparse-oldschool`	=
	(project in file("modules/oldschool"))
	.enablePlugins()
	.dependsOn()
	.settings(
		libraryDependencies	++= Seq(
			"de.djini"		%% "scutil-core"	% "0.167.0"	% "compile",
			"org.specs2"	%% "specs2-core"	% "4.8.1"	% "test"
		),
	)

lazy val `scparse-ng`	=
	(project in file("modules/ng"))
	.enablePlugins()
	.dependsOn()
	.settings(
		libraryDependencies	++= Seq(
			"de.djini"		%% "scutil-core"	% "0.167.0"	% "compile",
		),
	)


