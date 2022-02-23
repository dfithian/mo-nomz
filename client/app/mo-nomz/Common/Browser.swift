//
//  Browser.swift
//  mo-nomz
//
//  Created by Dan Fithian on 2/22/22.
//

import UIKit

class Browser {
    static func visitLink(_ link: String) {
        let url = URL(string: link)!
        visitUrl(url)
    }

    static func visitUrl(_ url: URL) {
        if #available(iOS 10.0, *) {
            UIApplication.shared.open(url, options: [:], completionHandler: nil)
        } else {
            UIApplication.shared.openURL(url)
        }
    }
}
