//
//  Keyboard.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/17/21.
//

import UIKit

extension UITextField {
    static let TOOLBAR_HEIGHT: CGFloat = 50

    @IBInspectable var doneAccessory: Bool {
        get { return self.doneAccessory }
        set (hasDone) {
            if hasDone{
                addDoneButtonOnKeyboard()
            }
        }
    }

    func addDoneButtonOnKeyboard() {
        let doneToolbar: UIToolbar = UIToolbar(frame: CGRect.init(x: 0, y: 0, width: UIScreen.main.bounds.width, height: UITextField.TOOLBAR_HEIGHT))
        doneToolbar.barStyle = .default
        
        let flexSpace = UIBarButtonItem(barButtonSystemItem: .flexibleSpace, target: nil, action: nil)
        let done: UIBarButtonItem = UIBarButtonItem(title: "Done", style: .done, target: self, action: #selector(self.doneButtonAction))
        
        let items = [flexSpace, done]
        doneToolbar.items = items
        doneToolbar.sizeToFit()
        self.inputAccessoryView = doneToolbar
    }
    
    @objc func doneButtonAction() {
        self.resignFirstResponder()
    }
}

extension UITextView {
    static let TOOLBAR_HEIGHT: CGFloat = 50

    @IBInspectable var doneAccessory: Bool {
        get { return self.doneAccessory }
        set (hasDone) {
            if hasDone{
                addDoneButtonOnKeyboard()
            }
        }
    }

    func addDoneButtonOnKeyboard() {
        let doneToolbar: UIToolbar = UIToolbar(frame: CGRect.init(x: 0, y: 0, width: UIScreen.main.bounds.width, height: UITextView.TOOLBAR_HEIGHT))
        doneToolbar.barStyle = .default
        
        let flexSpace = UIBarButtonItem(barButtonSystemItem: .flexibleSpace, target: nil, action: nil)
        let done: UIBarButtonItem = UIBarButtonItem(title: "Done", style: .done, target: self, action: #selector(self.doneButtonAction))
        
        let items = [flexSpace, done]
        doneToolbar.items = items
        doneToolbar.sizeToFit()
        self.inputAccessoryView = doneToolbar
    }
    
    @objc func doneButtonAction() {
        self.resignFirstResponder()
    }
}

extension UIViewController {
    static let MARGIN: CGFloat = 10

    func keyboardWillShowInternal(notification: NSNotification, keyboardView: UIView?, toolbarConstraint: NSLayoutConstraint?) {
        switch UIDevice.current.userInterfaceIdiom {
        case .phone:
            guard let keyboard = notification.userInfo?[UIResponder.keyboardFrameEndUserInfoKey] as? NSValue else { return }
            if let subview = keyboardView {
                view.frame.origin.y -= subview.frame.origin.y - UIViewController.MARGIN
                toolbarConstraint?.constant = subview.frame.origin.y - UIViewController.MARGIN - keyboard.cgRectValue.size.height
            } else {
                toolbarConstraint?.constant = UITextField.TOOLBAR_HEIGHT - 2 * UIViewController.MARGIN - keyboard.cgRectValue.size.height
            }
            view.layoutIfNeeded()
            break
        default: break
        }
    }
    
    func keyboardWillHideInternal(notification: NSNotification, toolbarConstraint: NSLayoutConstraint?) {
        switch UIDevice.current.userInterfaceIdiom {
        case .phone:
            view.frame.origin.y = 0
            toolbarConstraint?.constant = 0
            view.layoutIfNeeded()
            break
        default: break
        }
    }
}
