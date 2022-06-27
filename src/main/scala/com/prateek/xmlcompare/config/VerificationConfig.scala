package com.prateek.xmlcompare.config

type XPathRegex = String
type MVC = MessageVerificationConfig
type Verifiers = MessageVerificationConfig
type MessageVerificationConfig = (XPathRegex, Set[VerifierId])

case class VerificationConfig(discoverResponseConfig: Seq[MVC])

enum VerifierId:
  case Attribute, Child, Label, Text
  case Ignore // used for ignoring attribute key verification
  case Node // used only for NodeVerifier but is not queried for approval
