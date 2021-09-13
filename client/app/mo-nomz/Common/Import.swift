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
            if let name = url.lastPathComponent == "" ? nil : url.lastPathComponent {
                addBlob(content: content, name: name, link: nil, completion: nil)
            } else {
                addBlob(content: content, completion: nil)
            }
        } catch {
            print("Failed to get file \(error)")
        }
    }
    func importUrl(_ url: URL) {
        addLink(link: url.absoluteString, active: true, completion: nil)
    }
    func importData(_ text: String) {
        addBlob(content: text, completion: nil)
    }
}
