//
//  ProfileController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/12/21.
//

import UIKit

class ProfileController: UIViewController {
    @IBAction func didTapContactSupport(_ sender: Any) {
        if let url = URL(string: "mailto:\(Configuration.environment.contactEmail)") {
            if #available(iOS 10.0, *) {
                UIApplication.shared.open(url)
            } else {
                UIApplication.shared.openURL(url)
            }
        }
    }
}
