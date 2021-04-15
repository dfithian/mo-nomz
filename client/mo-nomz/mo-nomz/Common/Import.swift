//
//  Import.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/15/21.
//

import UIKit

extension UIViewController {
    func importFile(url: URL, completion: (() -> Void)?) {
        do {
            let name = url.lastPathComponent
            let content = try String(contentsOf: url, encoding: .utf8)
            addRecipeBody(name: name, content: content, completion: completion)
        } catch {
            print("Failed to get file \(error)")
        }
    }
    func importUrl(url: URL, completion: (() -> Void)?) {
        addRecipeLink(link: url.absoluteString, completion: completion)
    }
}
