package com.prateek.xmlcompare.config

type XPathRegex = String
//MessageVerificationConfig
type MVC = (XPathRegex, Set[VerifierId])

case class VerificationConfig(discoverResponse: Seq[MVC])

enum VerifierId:
  case Attribute, Child, Label, Text
  case Ignore // used for ignoring attribute key verification
  case Node // used only for NodeVerifier but is not queried for approval
