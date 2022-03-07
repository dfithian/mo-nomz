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
    
    func promptAcknowledgement(
        title: String,
        message: String,
        completion: @escaping ((UIAlertAction) -> Void)
    ) {
        let action = UIAlertAction(title: "Continue", style: .default, handler: completion)
        let confirmation = UIAlertController(title: title, message: message, preferredStyle: .alert)
        confirmation.addAction(action)
        DispatchQueue.main.async {
            self.present(confirmation, animated: true, completion: nil)
        }
    }
    
    func promptGetInput(
        title: String,
        completion: @escaping ((String) -> Void)
    ) {
        var text: UITextField? = nil
        let handler = { (_: UIAlertAction) -> Void in
            guard let str = text?.text else { return }
            completion(str)
        }
        let action = UIAlertAction(title: "OK", style: .default, handler: handler)
        let cancel = UIAlertAction(title: "Cancel", style: .cancel, handler: nil)
        let input = UIAlertController(title: title, message: nil, preferredStyle: .alert)
        input.addTextField(configurationHandler: { text = $0 })
        input.addAction(action)
        input.addAction(cancel)
        DispatchQueue.main.async {
            self.present(input, animated: true, completion: nil)
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
    func withCompletion(data: Data?, resp: URLResponse?, error: Error?, completion: ((Data) -> Void)?, onError: (() -> Void)) {
        if error == nil {
            if statusIsSuccessful(resp) {
                completion?(data!)
            } else {
                onError()
            }
        } else {
            onError()
        }
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
