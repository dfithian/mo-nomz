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
            let content = try String(contentsOf: url, encoding: .utf8)
            let name = url.lastPathComponent == "" ? nil : url.lastPathComponent
            addGroceryBlob(name: name, content: content, active: true, completion: nil)
        } catch {
            print("Failed to get file \(error)")
        }
    }
    func importUrl(_ url: URL) {
        addRecipeLink(link: url.absoluteString, active: true, completion: nil)
    }
    func importData(_ text: String) {
        addGroceryBlob(name: nil, content: text, active: true, completion: nil)
    }
}
