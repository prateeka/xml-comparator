package com.prateek.xmlcompare.verify

import com.prateek.xmlcompare.verify.VerifierId

type XPathRegex = String
type MVC = MessageVerificationConfig
type Verifiers = MessageVerificationConfig
type MessageVerificationConfig = (XPathRegex, Set[VerifierId])

case class VerificationConfig(discoverResponseConfig: Seq[MVC])
