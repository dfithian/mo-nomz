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
}
