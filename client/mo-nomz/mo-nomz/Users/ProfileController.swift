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
    @IBAction func didTapLogout(_ sender: Any) {
        let ok = UIAlertAction(title: "OK", style: .default, handler: { (action) -> Void in Persistence.clearState() })
        let cancel = UIAlertAction(title: "Cancel", style: .cancel, handler: nil)
        let confirmation = UIAlertController(title: "Logout", message: "Are you sure you want to logout? You will need to close and reopen the app to log back in.", preferredStyle: .alert)
        confirmation.addAction(ok)
        confirmation.addAction(cancel)
        self.present(confirmation, animated: true, completion: nil)
    }
}
