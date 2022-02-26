//
//  SimpleController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 2/3/22.
//

import UIKit

class SimpleController: UIViewController {
    @IBOutlet weak var keyboardView: UIView!
    @IBOutlet weak var toolbarConstraint: NSLayoutConstraint!
    
    @objc func keyboardWillShow(notification: NSNotification) {
        keyboardWillShowInternal(notification: notification, keyboardView: keyboardView, toolbarConstraint: toolbarConstraint)
    }
    
    @objc func keyboardWillHide(notification: NSNotification) {
        keyboardWillHideInternal(notification: notification, toolbarConstraint: toolbarConstraint)
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillShow), name: UIResponder.keyboardDidShowNotification, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillHide), name: UIResponder.keyboardWillHideNotification, object: nil)
    }
}
