//
//  ProfileController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/12/21.
//

import UIKit

class ProfileController: UIViewController {
    @IBAction func didTapContactSupport(_ sender: Any) {
        if let url = URL(string: Configuration.supportUrl) {
            if #available(iOS 10.0, *) {
                UIApplication.shared.open(url)
            } else {
                UIApplication.shared.openURL(url)
            }
        }
    }
    @IBAction func didTapLogout(_ sender: Any) {
        let handler = { (action: UIAlertAction) -> Void in Persistence.clearState() }
        promptForConfirmation(title: "Logout", message: "Are you sure you want to logout? You will need to close and reopen the app to log back in.", handler: handler)
    }
}
