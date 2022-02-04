//
//  ViewController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 2/3/22.
//

import UIKit

extension UIViewController {
    @IBAction func didTapCancel(_ sender: Any?) {
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
    }
}

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

    func buyPrompt(title: String, message: String, price: String, handler: @escaping ((UIAlertAction) -> Void)) {
        let ok = UIAlertAction(title: "Buy for \(price)", style: .default, handler: handler)
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

extension UIViewController {
    func needHelp() {
        if let url = URL(string: Configuration.helpUrl) {
            if #available(iOS 10.0, *) {
                UIApplication.shared.open(url)
            } else {
                UIApplication.shared.openURL(url)
            }
        }
    }
}

extension UIViewController {
    func withCompletion(data: Data?, resp: URLResponse?, error: Error?, completion: ((Data) -> Void)?, onUnsuccessfulStatus: ((URLResponse?) -> Void), onError: ((Error?) -> Void)) {
        if error == nil {
            if statusIsSuccessful(resp) {
                completion?(data!)
            } else {
                onUnsuccessfulStatus(resp)
            }
        } else {
            onError(error)
        }
    }

    func defaultWithCompletion(data: Data?, resp: URLResponse?, error: Error?, completion: ((Data) -> Void)?) {
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

extension UIViewController {
    func startLoading() -> UIActivityIndicatorView {
        let progress = UIActivityIndicatorView(style: .large)
        progress.translatesAutoresizingMaskIntoConstraints = true
        DispatchQueue.main.async {
            self.view.isUserInteractionEnabled = false
            progress.center = self.view.center
            progress.startAnimating()
            self.view.addSubview(progress)
        }
        return progress
    }
    
    func stopLoading(_ progress: UIActivityIndicatorView) {
        DispatchQueue.main.async {
            self.view.isUserInteractionEnabled = true
            progress.stopAnimating()
            progress.removeFromSuperview()
        }
    }
    
    func startProgress() -> UIProgressView {
        let progress = UIProgressView(progressViewStyle: .bar)
        progress.translatesAutoresizingMaskIntoConstraints = true
        DispatchQueue.main.async {
            self.view.isUserInteractionEnabled = false
            progress.trackTintColor = UIColor.lightGray
            progress.tintColor = UIColor.systemBlue
            progress.center = CGPoint(x: self.view.center.x, y: 2 * self.view.frame.size.height / 3)
            self.view.addSubview(progress)
        }
        return progress
    }
    
    func stopLoading(_ progress: UIProgressView) {
        DispatchQueue.main.async {
            self.view.isUserInteractionEnabled = true
            progress.removeFromSuperview()
        }
    }
}
