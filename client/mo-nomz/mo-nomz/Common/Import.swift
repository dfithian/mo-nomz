//
//  Import.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/15/21.
//

import UIKit

extension UIViewController {
    func importFile(_ url: URL) {
        do {
            let name = url.lastPathComponent
            let content = try String(contentsOf: url, encoding: .utf8)
            addRecipeBody(name: name, content: content, completion: nil)
        } catch {
            print("Failed to get file \(error)")
        }
    }
    func importUrl(_ url: URL) {
        addRecipeLink(link: url.absoluteString, completion: nil)
    }
    func importData(_ text: String) {
        addRecipeBody(name: "Import", content: text, completion: nil)
    }
}
