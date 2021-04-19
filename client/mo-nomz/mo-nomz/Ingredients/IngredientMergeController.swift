//
//  IngredientMergeController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/13/21.
//

import UIKit

class IngredientMergeController: UIViewController, UIPickerViewDataSource, UIPickerViewDelegate {
    var ingredientIds: [Int] = []
    var existing: ReadableIngredient = ReadableIngredient(name: "", quantity: ReadableQuantity(whole: nil, fraction: nil), unit: "", active: true)
    var new: ReadableIngredient = ReadableIngredient(name: "", quantity: ReadableQuantity(whole: nil, fraction: nil), unit: "", active: true)
    var onChange: (() -> Void)? = nil
    var currentWholeQuantity: Int? = nil
    var currentFractionQuantity: ReadableFraction? = nil
    
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
        let ingredient = ReadableIngredient(name: name.text!, quantity: ReadableQuantity(whole: currentWholeQuantity, fraction: currentFractionQuantity), unit: unit.text, active: existing.active)
        let completion = {
            DispatchQueue.main.async {
                self.dismiss(animated: true, completion: nil)
            }
            self.onChange?()
        }
        mergeIngredients(ingredientIds: ingredientIds, ingredient: ingredient, completion: completion)
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
    
    override func viewDidLoad() {
        super.viewDidLoad()
        existingInfo.text = existing.render()
        newInfo.text = new.render()
        unit.text = existing.unit
        name.text = existing.name
        let q = existing.unit == new.unit ? existing.quantity + new.quantity : existing.quantity
        currentWholeQuantity = q.whole
        currentFractionQuantity = q.fraction
        quantity.selectRow(q.whole ?? 0, inComponent: 0, animated: true)
        quantity.selectRow(q.fraction?.toInt() ?? 0, inComponent: 1, animated: true)
        name.addDoneButtonOnKeyboard()
        unit.addDoneButtonOnKeyboard()
    }
}
