//
//  Confirmation.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/17/21.
//

import UIKit

extension UIViewController {
    func promptForConfirmation(title: String, message: String, handler: @escaping ((UIAlertAction) -> Void)) {
        let ok = UIAlertAction(title: "OK", style: .default, handler: handler)
        let cancel = UIAlertAction(title: "Cancel", style: .cancel, handler: nil)
        let confirmation = UIAlertController(title: title, message: message, preferredStyle: .alert)
        confirmation.addAction(ok)
        confirmation.addAction(cancel)
        DispatchQueue.main.async {
            self.present(confirmation, animated: true, completion: nil)
        }
    }
    func promptForConfirmationThree(
        title: String,
        message: String,
        option1Button: String,
        option1Handler: @escaping ((UIAlertAction) -> Void),
        option2Button: String,
        option2Handler: @escaping ((UIAlertAction) -> Void)
    ) {
        let option1 = UIAlertAction(title: option1Button, style: .default, handler: option1Handler)
        let option2 = UIAlertAction(title: option2Button, style: .default, handler: option2Handler)
        let cancel = UIAlertAction(title: "Cancel", style: .cancel, handler: nil)
        let confirmation = UIAlertController(title: title, message: message, preferredStyle: .alert)
        confirmation.addAction(option1)
        confirmation.addAction(option2)
        confirmation.addAction(cancel)
        DispatchQueue.main.async {
            self.present(confirmation, animated: true, completion: nil)
        }
    }
}
