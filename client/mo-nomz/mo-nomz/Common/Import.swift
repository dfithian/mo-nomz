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
            addIngredientBlob(content: content, completion: nil)
        } catch {
            print("Failed to get file \(error)")
        }
    }
    func importUrl(_ url: URL) {
        addRecipeLink(link: url.absoluteString, completion: nil)
    }
    func importData(_ text: String) {
        addIngredientBlob(content: text, completion: nil)
    }
}
