//
//  SimpleController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 2/3/22.
//

import UIKit

class SimpleController: UIViewController {
    @IBOutlet weak var keyboardView: UIView!

    var beforeHeight: CGFloat? = nil
    
    @objc func keyboardWillShow(notification: NSNotification) {
        if let subview = keyboardView {
            beforeHeight = keyboardWillShowInternal(subview: subview, notification: notification)
        }
    }
    
    @objc func keyboardWillHide(notification: NSNotification) {
        keyboardWillHideInternal(heightMay: beforeHeight, notification: notification)
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillShow), name: UIResponder.keyboardDidShowNotification, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillHide), name: UIResponder.keyboardWillHideNotification, object: nil)
    }
}
