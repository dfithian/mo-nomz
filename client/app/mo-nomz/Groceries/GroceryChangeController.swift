//
//  GroceryChangeController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 1/24/22.
//

import UIKit

enum GroceryChange {
    case edit(ReadableGroceryItemWithId)
    case merge(ReadableGroceryItemWithId, ReadableGroceryItemWithId)
}

class GroceryChangeController: UIViewController, UIPickerViewDataSource, UIPickerViewDelegate {
    var change: GroceryChange? = nil
    var onChange: (() -> Void)? = nil
    var currentWholeQuantity: Int? = nil
    var currentFractionQuantity: ReadableFraction? = nil
    var beforeHeight: CGFloat? = nil
    
    @IBOutlet weak var heading: UILabel!
    @IBOutlet weak var synopsis: UILabel!
    @IBOutlet weak var quantity: UIPickerView!
    @IBOutlet weak var unit: UITextField!
    @IBOutlet weak var name: UITextField!
    @IBOutlet weak var existingInfo: UILabel!
    @IBOutlet weak var newInfo: UILabel!
    
    @IBAction func didTapCancel(_ sender: Any?) {
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
    }
    
    @IBAction func didTapSave(_ sender: Any?) {
        switch change {
        case .merge(let existing, let new):
            let item = ReadableGroceryItemWithId(item: ReadableGroceryItem(name: name.text!, quantity: ReadableQuantity(whole: currentWholeQuantity, fraction: currentFractionQuantity), unit: unit.text?.nonEmpty(), active: existing.item.active, order: existing.item.order), id: UUID())
            mergeGroceries(ids: [existing.id, new.id], grocery: item)
            break
        case .edit(let existing):
            let item = ReadableGroceryItem(name: name.text!, quantity: ReadableQuantity(whole: currentWholeQuantity, fraction: currentFractionQuantity), unit: unit.text?.nonEmpty(), active: existing.item.active, order: existing.item.order)
            updateGrocery(grocery: ReadableGroceryItemWithId(item: item, id: existing.id))
            break
        default: break
        }
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
        switch change {
        case .merge(let existing, let new):
            heading.text = "Merge"
            synopsis.text = "Merge ingredients"
            existingInfo.text = existing.item.render()
            newInfo.text = new.item.render()
            unit.text = existing.item.unit
            name.text = existing.item.name
            let q = existing.item.unit == new.item.unit ? existing.item.quantity + new.item.quantity : existing.item.quantity
            currentWholeQuantity = q.whole
            currentFractionQuantity = q.fraction
            quantity.selectRow(q.whole ?? 0, inComponent: 0, animated: true)
            quantity.selectRow(q.fraction?.toInt() ?? 0, inComponent: 1, animated: true)
            break
        case .edit(let existing):
            heading.text = "Edit"
            synopsis.text = "Edit ingredients"
            existingInfo.text = existing.item.render()
            newInfo.alpha = 0
            unit.text = existing.item.unit
            name.text = existing.item.name
            currentWholeQuantity = existing.item.quantity.whole
            currentFractionQuantity = existing.item.quantity.fraction
            quantity.selectRow(existing.item.quantity.whole ?? 0, inComponent: 0, animated: true)
            quantity.selectRow(existing.item.quantity.fraction?.toInt() ?? 0, inComponent: 1, animated: true)
            break
        default: break
        }
        unit.addDoneButtonOnKeyboard()
        name.addDoneButtonOnKeyboard()
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillShow), name: UIResponder.keyboardDidShowNotification, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillHide), name: UIResponder.keyboardWillHideNotification, object: nil)
    }
}