//
//  GroceryAddControllers.swift
//  mo-nomz
//
//  Created by Dan Fithian on 1/16/22.
//

import UIKit

class GroceryAddBlobController: UIViewController {
    @IBOutlet weak var blob: UITextView!
    @IBOutlet weak var checkbox: UIButton!
    @IBOutlet weak var submit: UIBarButtonItem!
    @IBOutlet weak var switcher: UIButton!
    var isRecipe: Bool = false
    var navigationVc: AddController? = nil
    var beforeHeight: CGFloat? = nil
    
    @IBAction func didTapCancel(_ sender: Any?) {
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
    }
    
    @IBAction func didTapSubmit(_ sender: Any?) {
        if let content = blob.text, !content.isEmpty {
            if isRecipe {
                performSegue(withIdentifier: "pushManualRecipe", sender: nil)
            } else {
                let completion = {
                    DispatchQueue.main.async {
                        self.dismiss(animated: true, completion: nil)
                    }
                    self.navigationVc?.onChange?()
                }
                addBlob(content: content, completion: completion)
            }
        } else {
            alertUnsuccessful("Please provide ingredients.")
        }
    }
    
    @IBAction func didTapIsRecipe(_ sender: Any?) {
        if isRecipe {
            isRecipe = false
            checkbox.setImage(UIImage(systemName: "square"), for: .normal)
            submit.title = "Submit"
        } else {
            isRecipe = true
            checkbox.setImage(UIImage(systemName: "checkmark.square"), for: .normal)
            submit.title = "Next"
        }
    }
    
    @objc func keyboardWillShow(notification: NSNotification) {
        beforeHeight = keyboardWillShowInternal(subview: blob, notification: notification)
    }
    
    @objc func keyboardWillHide(notification: NSNotification) {
        keyboardWillHideInternal(heightMay: beforeHeight, notification: notification)
    }
    
    private func setupSwitcher() {
        switcher.menu = UIMenu(title: "Try another way", options: .displayInline, children: [
            UIAction(title: "Add link", image: UIImage(systemName: "link"), state: .off, handler: { _ in self.navigationVc?.switchToLink() }),
            UIAction(title: "Add manual", image: UIImage(systemName: "pencil"), state: .on, handler: { _ in self.navigationVc?.switchToManual() })
        ])
        switcher.showsMenuAsPrimaryAction = true
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? GroceryAddRecipeController, segue.identifier == "pushManualRecipe" {
            vc.blob = blob.text
        }
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillShow), name: UIResponder.keyboardDidShowNotification, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillHide), name: UIResponder.keyboardWillHideNotification, object: nil)
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        blob.addDoneButtonOnKeyboard()
        blob.layer.cornerRadius = 10
        setupSwitcher()
    }
}

class GroceryAddRecipeController: UIViewController {
    @IBOutlet weak var name: UITextField!
    @IBOutlet weak var link: UITextField!
    @IBOutlet weak var active: UIButton!
    @IBOutlet weak var steps: UITextView!
    @IBOutlet weak var switcher: UIButton!
    var blob: String? = nil
    var isActive: Bool = true
    var navigationVc: AddController? = nil
    var beforeHeight: CGFloat? = nil
    
    @IBAction func didTapCancel(_ sender: Any?) {
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
    }
    
    @IBAction func didTapBack(_ sender: Any?) {
        navigationVc?.popViewController(animated: true)
    }
    
    @IBAction func didTapSubmit(_ sender: Any?) {
        if let name = name.text?.nonEmpty() {
            let link = link.text?.nonEmpty()
            let steps = (steps.text ?? "").components(separatedBy: "\n").compactMap({ $0.nonEmpty() })
            let completion = {
                DispatchQueue.main.async {
                    self.dismiss(animated: true, completion: nil)
                }
                self.navigationVc?.onChange?()
            }
            addBlob(content: self.blob!, name: name, link: link, rawSteps: steps, active: isActive, completion: completion)
        } else {
            alertUnsuccessful("Please provide a name.")
        }
    }
    
    @IBAction func didTapIsActive(_ sender: Any?) {
        if isActive {
            isActive = false
            active.setImage(UIImage(systemName: "square"), for: .normal)
        } else {
            isActive = true
            active.setImage(UIImage(systemName: "checkmark.square"), for: .normal)
        }
    }
    
    @objc func keyboardWillShow(notification: NSNotification) {
        beforeHeight = keyboardWillShowInternal(subview: steps, notification: notification)
    }
    
    @objc func keyboardWillHide(notification: NSNotification) {
        keyboardWillHideInternal(heightMay: beforeHeight, notification: notification)
    }
    
    private func setupSwitcher() {
        switcher.menu = UIMenu(title: "Try another way", options: .displayInline, children: [
            UIAction(title: "Add link", image: UIImage(systemName: "link"), state: .off, handler: { _ in self.navigationVc?.switchToLink() }),
            UIAction(title: "Add manual", image: UIImage(systemName: "pencil"), state: .on, handler: { _ in self.navigationVc?.switchToManual() })
        ])
        switcher.showsMenuAsPrimaryAction = true
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillShow), name: UIResponder.keyboardDidShowNotification, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillHide), name: UIResponder.keyboardWillHideNotification, object: nil)
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        steps.addDoneButtonOnKeyboard()
        steps.layer.cornerRadius = 10
        name.addDoneButtonOnKeyboard()
        link.addDoneButtonOnKeyboard()
        setupSwitcher()
    }
}
