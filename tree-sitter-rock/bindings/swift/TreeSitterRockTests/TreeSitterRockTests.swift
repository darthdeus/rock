import XCTest
import SwiftTreeSitter
import TreeSitterRock

final class TreeSitterRockTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_rock())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Rock grammar")
    }
}
