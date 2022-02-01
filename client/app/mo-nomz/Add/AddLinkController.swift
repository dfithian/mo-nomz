//
//  AddLinkController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/12/21.
//

import UIKit
import SafariServices

class AddLinkController: UIViewController {
    @IBOutlet weak var link: UITextField!
    @IBOutlet weak var checkbox: UIButton!
    @IBOutlet weak var switcher: UIBarButtonItem!
    var active: Bool = true
    var navigationVc: AddController? = nil
    var beforeHeight: CGFloat? = nil
    
    @IBAction func didTapCancel(_ sender: Any?) {
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
    }
    
    @IBAction func didTapSubmit(_ sender: Any?) {
        let completion = { () -> Void in
            DispatchQueue.main.async {
                self.dismiss(animated: true, completion: nil)
            }
            self.navigationVc?.onChange?()
        }
        if let newLink = link.text?.nonEmpty() {
            addLink(link: newLink, active: active, completion: completion)
        } else {
            alertUnsuccessful("Please provide a link.")
        }
    }
    
    @IBAction func didTapIsActive(_ sender: Any?) {
        if active {
            active = false
            checkbox.setImage(UIImage(systemName: "square"), for: .normal)
        } else {
            active = true
            checkbox.setImage(UIImage(systemName: "checkmark.square"), for: .normal)
        }
    }
    
    @objc func keyboardWillShow(notification: NSNotification) {
        beforeHeight = keyboardWillShowInternal(subview: link, notification: notification)
    }
    
    @objc func keyboardWillHide(notification: NSNotification) {
        keyboardWillHideInternal(heightMay: beforeHeight, notification: notification)
    }
    
    private func setupSwitcher() {
        switcher.menu = UIMenu(options: .displayInline, children: [
            UIAction(title: "Add link", image: UIImage(systemName: "link"), state: .on, handler: { _ in self.navigationVc?.switchToLink() }),
            UIAction(title: "Add manual", image: UIImage(systemName: "pencil"), state: .off, handler: { _ in self.navigationVc?.switchToManual() }),
            UIAction(title: "Add photo", image: UIImage(systemName: "photo.on.rectangle"), state: .off, handler: { _ in self.navigationVc?.switchToPhoto() })
        ])
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillShow), name: UIResponder.keyboardDidShowNotification, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillHide), name: UIResponder.keyboardWillHideNotification, object: nil)
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        link.addDoneButtonOnKeyboard()
        setupSwitcher()
    }
}
