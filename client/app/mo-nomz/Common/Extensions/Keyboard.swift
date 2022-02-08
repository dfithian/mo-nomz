//
//  Keyboard.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/17/21.
//

import UIKit

extension UITextField {
    @IBInspectable var doneAccessory: Bool {
        get { return self.doneAccessory }
        set (hasDone) {
            if hasDone{
                addDoneButtonOnKeyboard()
            }
        }
    }

    func addDoneButtonOnKeyboard() {
        let doneToolbar: UIToolbar = UIToolbar(frame: CGRect.init(x: 0, y: 0, width: UIScreen.main.bounds.width, height: 50))
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
    @IBInspectable var doneAccessory: Bool {
        get { return self.doneAccessory }
        set (hasDone) {
            if hasDone{
                addDoneButtonOnKeyboard()
            }
        }
    }

    func addDoneButtonOnKeyboard() {
        let doneToolbar: UIToolbar = UIToolbar(frame: CGRect.init(x: 0, y: 0, width: UIScreen.main.bounds.width, height: 50))
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
    static let margin: CGFloat = 10

    func keyboardWillShowInternal(subview: UIView, notification: NSNotification) -> CGFloat? {
        guard let keyboard = notification.userInfo?[UIResponder.keyboardFrameEndUserInfoKey] as? NSValue else { return nil }
        if self.view.frame.origin.y == 0 {
            let height = self.view.frame.height
            self.view.frame.origin.y -= (subview.frame.origin.y - UIViewController.margin)
            self.view.frame.size.height -= (keyboard.cgRectValue.height - subview.frame.origin.y + UIViewController.margin)
            return height
        }
        return nil
    }
    
    func keyboardWillHideInternal(heightMay: CGFloat?, notification: NSNotification) {
        guard let keyboard = notification.userInfo?[UIResponder.keyboardFrameEndUserInfoKey] as? NSValue else { return }
        if self.view.frame.origin.y != 0 {
            if let height = heightMay {
                self.view.frame.size.height = height
            } else {
                self.view.frame.size.height += (keyboard.cgRectValue.height + self.view.frame.origin.y - UIViewController.margin)
            }
            self.view.frame.origin.y = 0
        }
    }
}
