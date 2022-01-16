//
//  GroceryAddControllers.swift
//  mo-nomz
//
//  Created by Dan Fithian on 1/16/22.
//

import UIKit

class GroceryAddBlobController: UIViewController, Submit {
    @IBOutlet weak var blob: UITextView!
    @IBOutlet weak var checkbox: UIButton!
    var onIsRecipe: ((Bool) -> Void)? = nil
    var isRecipe: Bool = false
    var beforeHeight: CGFloat? = nil
    
    @IBAction func didTapIsRecipe(_ sender: Any?) {
        if isRecipe {
            isRecipe = false
            checkbox.setImage(UIImage(systemName: "square"), for: .normal)
            onIsRecipe?(false)
        } else {
            isRecipe = true
            checkbox.setImage(UIImage(systemName: "checkmark.square"), for: .normal)
            onIsRecipe?(true)
        }
    }
    
    func submit(_ onChange: (() -> Void)?) {
        if let content = blob.text, !content.isEmpty {
            if isRecipe {
                performSegue(withIdentifier: "pushManualRecipe", sender: nil)
            } else {
                let completion = {
                    DispatchQueue.main.async {
                        self.dismiss(animated: true, completion: nil)
                    }
                    onChange?()
                }
                addBlob(content: content, completion: completion)
            }
        } else {
            alertUnsuccessful("Please provide ingredients.")
        }
    }
    
    @objc func keyboardWillShow(notification: NSNotification) {
        beforeHeight = keyboardWillShowInternal(subview: blob, notification: notification)
    }
    
    @objc func keyboardWillHide(notification: NSNotification) {
        keyboardWillHideInternal(heightMay: beforeHeight, notification: notification)
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
    }
}

class GroceryAddRecipeController: UIViewController, Submit {
    @IBOutlet weak var name: UITextField!
    @IBOutlet weak var link: UITextField!
    @IBOutlet weak var active: UIButton!
    @IBOutlet weak var steps: UITextView!
    var blob: String? = nil
    var isActive: Bool = true
    var beforeHeight: CGFloat? = nil
    
    @IBAction func didTapIsActive(_ sender: Any?) {
        if isActive {
            isActive = false
            active.setImage(UIImage(systemName: "square"), for: .normal)
        } else {
            isActive = true
            active.setImage(UIImage(systemName: "checkmark.square"), for: .normal)
        }
    }
    
    func submit(_ onChange: (() -> Void)?) {
        if let name = name.text?.nonEmpty() {
            let link = link.text?.nonEmpty()
            let steps = (steps.text ?? "").components(separatedBy: "\n").compactMap({ $0.nonEmpty() })
            let completion = {
                DispatchQueue.main.async {
                    self.dismiss(animated: true, completion: nil)
                }
                onChange?()
            }
            addBlob(content: self.blob!, name: name, link: link, rawSteps: steps, active: isActive, completion: completion)
        } else {
            alertUnsuccessful("Please provide a name.")
        }
    }
    
    @objc func keyboardWillShow(notification: NSNotification) {
        beforeHeight = keyboardWillShowInternal(subview: steps, notification: notification)
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
        steps.addDoneButtonOnKeyboard()
        steps.layer.cornerRadius = 10
        name.addDoneButtonOnKeyboard()
        link.addDoneButtonOnKeyboard()
    }
}
