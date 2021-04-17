//
//  ErrorHandler.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/14/21.
//

import UIKit

extension UIViewController {
    func defaultOnError(error: Error?) {
        let message: String
        switch Configuration.environment {
        case .Release:
            message = "Mo Nomz is not available right now. Please try again later."
            break
        default:
            message = "\(error as Any)"
            break
        }
        let ok = UIAlertAction(title: "OK", style: .default, handler: nil)
        let confirmation = UIAlertController(title: "Error", message: message, preferredStyle: .alert)
        confirmation.addAction(ok)
        DispatchQueue.main.async {
            self.present(confirmation, animated: true, completion: nil)
        }
    }
}
