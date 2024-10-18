// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "TreeSitterRock",
    products: [
        .library(name: "TreeSitterRock", targets: ["TreeSitterRock"]),
    ],
    dependencies: [
        .package(url: "https://github.com/ChimeHQ/SwiftTreeSitter", from: "0.8.0"),
    ],
    targets: [
        .target(
            name: "TreeSitterRock",
            dependencies: [],
            path: ".",
            sources: [
                "src/parser.c",
                // NOTE: if your language has an external scanner, add it here.
            ],
            resources: [
                .copy("queries")
            ],
            publicHeadersPath: "bindings/swift",
            cSettings: [.headerSearchPath("src")]
        ),
        .testTarget(
            name: "TreeSitterRockTests",
            dependencies: [
                "SwiftTreeSitter",
                "TreeSitterRock",
            ],
            path: "bindings/swift/TreeSitterRockTests"
        )
    ],
    cLanguageStandard: .c11
)
