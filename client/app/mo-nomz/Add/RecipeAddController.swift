//
//  RecipeAddController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/12/21.
//

import UIKit
import SafariServices

class RecipeAddController: UIViewController, Submit {
    @IBOutlet weak var link: UITextField!
    @IBOutlet weak var checkbox: UIButton!
    var existingLinks: [String] = []
    var active: Bool = true
    var beforeHeight: CGFloat? = nil
    
    @IBAction func didTapIsActive(_ sender: Any?) {
        if active {
            active = false
            checkbox.setImage(UIImage(systemName: "square"), for: .normal)
        } else {
            active = true
            checkbox.setImage(UIImage(systemName: "checkmark.square"), for: .normal)
        }
    }
    
    func submit(_ onChange: (() -> Void)?) {
        let completion = { () -> Void in
            DispatchQueue.main.async {
                self.dismiss(animated: true, completion: nil)
            }
            onChange?()
        }
        if let newLink = link.text?.nonEmpty() {
            addLink(link: newLink, active: active, completion: completion)
        } else {
            alertUnsuccessful("Please add a link.")
        }
    }
    
    @objc func keyboardWillShow(notification: NSNotification) {
        beforeHeight = keyboardWillShowInternal(subview: link, notification: notification)
    }
    
    @objc func keyboardWillHide(notification: NSNotification) {
        keyboardWillHideInternal(heightMay: beforeHeight, notification: notification)
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillShow), name: UIResponder.keyboardDidShowNotification, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillHide), name: UIResponder.keyboardWillHideNotification, object: nil)
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        link.addDoneButtonOnKeyboard()
    }
}
