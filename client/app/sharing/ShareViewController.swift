//
//  ShareViewController.swift
//  sharing
//
//  Created by Dan Fithian on 4/20/21.
//

import UIKit
import Social
import MobileCoreServices

class ShareViewController: SLComposeServiceViewController {
    let textType: String = kUTTypeText as String
    let urlType: String = kUTTypeURL as String

    override func isContentValid() -> Bool {
        if let attachment = (self.extensionContext?.inputItems.first as? NSExtensionItem)?.attachments?.first {
            return attachment.hasItemConformingToTypeIdentifier(textType) || attachment.hasItemConformingToTypeIdentifier(urlType)
        }
        return false
    }

    override func didSelectPost() {
        if let attachment = (self.extensionContext?.inputItems.first as? NSExtensionItem)?.attachments?.first {
            if attachment.hasItemConformingToTypeIdentifier(textType) {
                attachment.loadItem(forTypeIdentifier: textType, options: nil, completionHandler: { (data, error) in
                    guard error == nil else { return }
                    if let url = data as? URL {
                        print("Importing file")
                        self.importFile(url)
                    } else if let text = data as? String {
                        print("Importing data")
                        self.importData(text)
                    }
                })
            } else if attachment.hasItemConformingToTypeIdentifier(urlType) {
                attachment.loadItem(forTypeIdentifier: urlType, options: nil, completionHandler: { (data, error) in
                    guard error == nil else { return }
                    if let url = data as? URL {
                        print("Importing URL")
                        self.importUrl(url)
                    }
                })
            }
        }
        self.extensionContext!.completeRequest(returningItems: [], completionHandler: nil)
    }

    override func configurationItems() -> [Any]! {
        return []
    }
}
