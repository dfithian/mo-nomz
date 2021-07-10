//
//  ErrorHandler.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/14/21.
//

import UIKit

extension UIViewController {
    func withCompletion(data: Data?, resp: URLResponse?, error: Error?, completion: (() -> Void)?, onUnsuccessfulStatus: ((URLResponse?) -> Void), onError: ((Error?) -> Void)) {
        if error == nil {
            if statusIsSuccessful(resp) {
                completion?()
            } else {
                onUnsuccessfulStatus(resp)
            }
        } else {
            onError(error)
        }
    }

    func defaultWithCompletion(data: Data?, resp: URLResponse?, error: Error?, completion: (() -> Void)?) {
        withCompletion(data: data, resp: resp, error: error, completion: completion, onUnsuccessfulStatus: defaultOnUnsuccessfulStatus, onError: defaultOnError)
    }

    func statusIsSuccessful(_ resp: URLResponse?) -> Bool {
        if let statusCode = (resp as? HTTPURLResponse)?.statusCode {
            return statusCode >= 200 && statusCode < 300
        }
        return false
    }
    
    func alertUnsuccessful(_ message: String) {
        let ok = UIAlertAction(title: "OK", style: .default, handler: nil)
        let confirmation = UIAlertController(title: "Error", message: message, preferredStyle: .alert)
        confirmation.addAction(ok)
        DispatchQueue.main.async {
            self.present(confirmation, animated: true, completion: nil)
        }
    }
    
    func defaultOnUnsuccessfulStatus(_ resp: URLResponse?) {
        let message: String
        switch Configuration.environment {
        case .Release:
            message = "An error occurred."
            break
        default:
            message = "\(resp as Any)"
            break
        }
        alertUnsuccessful(message)
    }
    
    func onParseError(_ resp: URLResponse?) {
        let message: String
        switch Configuration.environment {
        case .Release:
            message = "Couldn't parse items. Please try to format items as they'd appear on a grocery list."
            break
        default:
            message = "\(resp as Any)"
            break
        }
        alertUnsuccessful(message)
    }

    func defaultOnError(_ error: Error?) {
        let message: String
        switch Configuration.environment {
        case .Release:
            message = "Nomz is not available right now. Please try again later."
            break
        default:
            message = "\(error as Any)"
            break
        }
        alertUnsuccessful(message)
    }
}
