package com.prateek.xmlcompare.read

import org.scalatest.funspec.AnyFunSpec

class MessageSpec extends AnyFunSpec {

  describe("xml containing DiscoverResponse") {
    it("should return the 'root' as comparison node") {
      val node =
        <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
          <soap:Body>
            <DiscoverResponse xmlns="urn:schemas-microsoft-com:xml-analysis">
              <return>
                <root xmlns="urn:schemas-microsoft-com:xml-analysis:rowset">
                  <row>
                    <CATALOG_NAME/>
                    <IS_DATAMEMBER>false</IS_DATAMEMBER>
                  </row>
                </root>
              </return>
            </DiscoverResponse>
          </soap:Body>
        </soap:Envelope>
      node match {
        case DiscoverResponse.Applied((node, msg)) =>
          assertResult("root")(node.label)
          assertResult(DiscoverResponse)(msg)
      }
    }
  }
}
