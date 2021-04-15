//
//  Import.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/15/21.
//

import Foundation

class Import {
    static func importFile(url: URL, completion: (() -> Void)?, onError: ((Error?) -> Void)?) {
        do {
            let name = url.lastPathComponent
            let content = try String(contentsOf: url, encoding: .utf8)
            Actions.addRecipeBody(name: name, content: content, completion: completion, onError: onError)
        } catch {
            print("Failed to get file \(error)")
        }
    }
    static func importUrl(url: URL, completion: (() -> Void)?, onError: ((Error?) -> Void)?) {
        Actions.addRecipeLink(link: url.absoluteString, completion: completion, onError: onError)
    }
}
