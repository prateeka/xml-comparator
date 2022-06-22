package com.prateek.xmlcompare.verify

import com.prateek.xmlcompare.verify.Identification.VerifierId

case class VerificationConfig(discoverResponseConfig: MVC)

type MVC = MessageVerificationConfig

case class MessageVerificationConfig(
    xpath: String,
    verifierIds: Seq[VerifierId]
)
