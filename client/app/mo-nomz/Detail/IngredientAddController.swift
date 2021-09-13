//
//  IngredientAddController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/13/21.
//

import UIKit

class IngredientAddController: UIViewController, UIPickerViewDataSource, UIPickerViewDelegate {
    var recipe: ReadableRecipeWithId? = nil
    var order: Int? = nil
    var onChange: (() -> Void)? = nil
    var currentWholeQuantity: Int? = nil
    var currentFractionQuantity: ReadableFraction? = nil
    var beforeHeight: CGFloat? = nil
    
    @IBOutlet weak var quantity: UIPickerView!
    @IBOutlet weak var unit: UITextField!
    @IBOutlet weak var name: UITextField!
    
    @IBAction func didTapCancel(_ sender: Any?) {
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
    }
    
    @IBAction func didTapSave(_ sender: Any?) {
        guard let r = recipe else { return }
        let item = ReadableIngredientWithId(id: UUID(), ingredient: ReadableIngredient(name: name.text!, quantity: ReadableQuantity(whole: currentWholeQuantity, fraction: currentFractionQuantity), unit: unit.text?.nonEmpty(), order: order ?? 1))
        updateRecipeIngredients(id: r.id, active: r.recipe.active, deletes: [], adds: [item])
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
        onChange?()
    }
    
    func pickerView(_ pickerView: UIPickerView, didSelectRow row: Int, inComponent component: Int) {
        switch component {
        case 0:
            currentWholeQuantity = row
            break
        default:
            currentFractionQuantity = ReadableFraction.fromInt(x: row)
            break
        }
    }
    
    func numberOfComponents(in pickerView: UIPickerView) -> Int {
        return 2
    }
    
    func pickerView(_ pickerView: UIPickerView, numberOfRowsInComponent component: Int) -> Int {
        switch component {
        case 0: return 100
        default: return 6
        }
    }
    
    func pickerView(_ pickerView: UIPickerView, titleForRow row: Int, forComponent component: Int) -> String? {
        switch component {
        case 0: return String(row)
        default: return ReadableFraction.fromInt(x: row)?.render() ?? "0"
        }
    }
    
    @objc func keyboardWillShow(notification: NSNotification) {
        beforeHeight = keyboardWillShowInternal(subview: name, notification: notification)
    }
    
    @objc func keyboardWillHide(notification: NSNotification) {
        keyboardWillHideInternal(heightMay: beforeHeight, notification: notification)
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        currentWholeQuantity = 1
        quantity.selectRow(1, inComponent: 0, animated: true)
        quantity.selectRow(0, inComponent: 1, animated: true)
        unit.addDoneButtonOnKeyboard()
        name.addDoneButtonOnKeyboard()
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillShow), name: UIResponder.keyboardDidShowNotification, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillHide), name: UIResponder.keyboardWillHideNotification, object: nil)
    }
}
