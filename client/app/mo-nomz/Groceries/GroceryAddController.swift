//
//  GroceryAddController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/23/21.
//

import UIKit

class GroceryAddController: UIViewController {
    var onChange: (() -> Void)? = nil
    var linkVc: RecipeAddController? = nil
    var bulkVc: GroceryAddBlobController? = nil
    var linkBeforeHeight: CGFloat? = nil
    var blobBeforeHeight: CGFloat? = nil
    
    @IBOutlet weak var segment: UISegmentedControl!
    @IBOutlet weak var linkView: UIView!
    @IBOutlet weak var bulkView: UIView!

    @IBAction func didTapCancel(_ sender: Any?) {
        cancel()
    }
    
    @IBAction func didTapSave(_ sender: Any?) {
        switch segment.selectedSegmentIndex {
        case 0:
            linkVc?.save(onChange, onCancel: cancel)
        default:
            bulkVc?.save(onChange, onCancel: cancel)
        }
    }
    
    @IBAction func didTapSegment(_ sender: UISegmentedControl) {
        switch sender.selectedSegmentIndex {
        case 0:
            linkView.alpha = 1
            bulkView.alpha = 0
            break
        default:
            linkView.alpha = 0
            bulkView.alpha = 1
            break
        }
    }
    
    func cancel() {
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? RecipeAddController, segue.identifier == "embedLink" {
            linkVc = vc
        }
        if let vc = segue.destination as? GroceryAddBlobController, segue.identifier == "embedBulk" {
            bulkVc = vc
        }
    }
    
    @objc func keyboardWillShow(notification: NSNotification) {
        if linkView.alpha == 1 {
            linkBeforeHeight = keyboardWillShowInternal(subview: linkVc!.link, notification: notification)
            for constraint in linkVc!.onKeyboardHideConstraints {
                constraint.isActive = false
            }
            for constraint in linkVc!.onKeyboardShowConstraints {
                constraint.isActive = true
            }
        } else {
            blobBeforeHeight = keyboardWillShowInternal(subview: bulkVc!.blob, notification: notification)
            for constraint in bulkVc!.onKeyboardHideConstraints {
                constraint.isActive = false
            }
            for constraint in bulkVc!.onKeyboardShowConstraints {
                constraint.isActive = true
            }
        }
    }
    
    @objc func keyboardWillHide(notification: NSNotification) {
        if linkView.alpha == 1 {
            keyboardWillHideInternal(heightMay: linkBeforeHeight, notification: notification)
            for constraint in linkVc!.onKeyboardHideConstraints {
                constraint.isActive = true
            }
            for constraint in linkVc!.onKeyboardShowConstraints {
                constraint.isActive = false
            }
        } else {
            keyboardWillHideInternal(heightMay: blobBeforeHeight, notification: notification)
            for constraint in bulkVc!.onKeyboardHideConstraints {
                constraint.isActive = true
            }
            for constraint in bulkVc!.onKeyboardShowConstraints {
                constraint.isActive = false
            }
        }
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        didTapSegment(segment)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillShow), name: UIResponder.keyboardDidShowNotification, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillHide), name: UIResponder.keyboardWillHideNotification, object: nil)
    }
}
