//
//  GroceryEditController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/13/21.
//

import UIKit

class GroceryEditController: UIViewController, UIPickerViewDataSource, UIPickerViewDelegate {
    var existing: ReadableGroceryItemWithId? = nil
    var onChange: (() -> Void)? = nil
    var currentWholeQuantity: Int? = nil
    var currentFractionQuantity: ReadableFraction? = nil
    var beforeHeight: CGFloat? = nil
    
    @IBOutlet weak var quantity: UIPickerView!
    @IBOutlet weak var unit: UITextField!
    @IBOutlet weak var name: UITextField!
    @IBOutlet weak var existingInfo: UILabel!
    
    @IBAction func didTapCancel(_ sender: Any?) {
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
    }
    
    @IBAction func didTapSave(_ sender: Any?) {
        guard let e = existing else { return }
        let item = ReadableGroceryItem(name: name.text!, quantity: ReadableQuantity(whole: currentWholeQuantity, fraction: currentFractionQuantity), unit: unit.text, active: e.item.active, order: e.item.order)
        let completion = {
            DispatchQueue.main.async {
                self.dismiss(animated: true, completion: nil)
            }
            self.onChange?()
        }
        updateGroceryItem(id: e.id, grocery: item, completion: completion)
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
        if let e = existing {
            existingInfo.text = e.item.render()
            unit.text = e.item.unit
            name.text = e.item.name
            currentWholeQuantity = e.item.quantity.whole
            currentFractionQuantity = e.item.quantity.fraction
            quantity.selectRow(e.item.quantity.whole ?? 0, inComponent: 0, animated: true)
            quantity.selectRow(e.item.quantity.fraction?.toInt() ?? 0, inComponent: 1, animated: true)
        }
        unit.addDoneButtonOnKeyboard()
        name.addDoneButtonOnKeyboard()
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillShow), name: UIResponder.keyboardDidShowNotification, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillHide), name: UIResponder.keyboardWillHideNotification, object: nil)
    }
}
