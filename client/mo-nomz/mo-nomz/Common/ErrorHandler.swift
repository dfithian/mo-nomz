//
//  ErrorHandler.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/14/21.
//

import UIKit

extension UIViewController {
    func defaultOnError() {
        let ok = UIAlertAction(title: "OK", style: .default, handler: nil)
        let confirmation = UIAlertController(title: "Error", message: "Mo Nomz is not available right now. Please try again later.", preferredStyle: .alert)
        confirmation.addAction(ok)
        DispatchQueue.main.async {
            self.present(confirmation, animated: true, completion: nil)
        }
    }
}
