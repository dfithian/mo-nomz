//
//  IngredientChangeController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 1/24/22.
//

import UIKit

enum IngredientChange {
    case add(Int)
    case edit(ReadableIngredientWithId)
    case merge(ReadableIngredientWithId, ReadableIngredientWithId)
}

class IngredientChangeController: SimpleController, UIPickerViewDataSource, UIPickerViewDelegate {
    var recipe: ReadableRecipeWithId? = nil
    var change: IngredientChange? = nil
    var onChange: (() -> Void)? = nil
    var currentWholeQuantity: Int? = nil
    var currentFractionQuantity: ReadableFraction? = nil
    
    @IBOutlet weak var heading: UILabel!
    @IBOutlet weak var synopsis: UILabel!
    @IBOutlet weak var quantity: UIPickerView!
    @IBOutlet weak var unit: UITextField!
    @IBOutlet weak var name: UITextField!
    @IBOutlet weak var existingInfo: UILabel!
    @IBOutlet weak var newInfo: UILabel!
    
    @IBAction func didTapSave(_ sender: Any?) {
        guard let r = recipe else { return }
        switch change {
        case .merge(let existing, let new):
            let item = ReadableIngredientWithId(id: UUID(), ingredient: ReadableIngredient(name: name.text!, quantity: ReadableQuantity(whole: currentWholeQuantity, fraction: currentFractionQuantity), unit: unit.text?.nonEmpty(), order: existing.ingredient.order))
            Database.updateRecipeIngredients(id: r.id, active: r.recipe.active, deletes: [existing.id, new.id], adds: [item])
            break
        case .edit(let existing):
            let item = ReadableIngredientWithId(id: UUID(), ingredient: ReadableIngredient(name: name.text!, quantity: ReadableQuantity(whole: currentWholeQuantity, fraction: currentFractionQuantity), unit: unit.text?.nonEmpty(), order: existing.ingredient.order))
            Database.updateRecipeIngredients(id: r.id, active: r.recipe.active, deletes: [existing.id], adds: [item])
            break
        case .add(let order):
            let item = ReadableIngredientWithId(id: UUID(), ingredient: ReadableIngredient(name: name.text!, quantity: ReadableQuantity(whole: currentWholeQuantity, fraction: currentFractionQuantity), unit: unit.text?.nonEmpty(), order: order))
            Database.updateRecipeIngredients(id: r.id, active: r.recipe.active, deletes: [], adds: [item])
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
    
    override func viewDidLoad() {
        super.viewDidLoad()
        switch change {
        case .merge(let existing, let new):
            heading.text = "Merge"
            synopsis.text = "Merge ingredients"
            existingInfo.text = existing.ingredient.render()
            newInfo.text = new.ingredient.render()
            unit.text = existing.ingredient.unit
            name.text = existing.ingredient.name
            let q = existing.ingredient.unit == new.ingredient.unit ? existing.ingredient.quantity + new.ingredient.quantity : existing.ingredient.quantity
            currentWholeQuantity = q.whole
            currentFractionQuantity = q.fraction
            quantity.selectRow(q.whole ?? 0, inComponent: 0, animated: true)
            quantity.selectRow(q.fraction?.toInt() ?? 0, inComponent: 1, animated: true)
            break
        case .edit(let existing):
            heading.text = "Edit"
            synopsis.text = "Edit ingredients"
            existingInfo.text = existing.ingredient.render()
            newInfo.alpha = 0
            unit.text = existing.ingredient.unit
            name.text = existing.ingredient.name
            currentWholeQuantity = existing.ingredient.quantity.whole
            currentFractionQuantity = existing.ingredient.quantity.fraction
            quantity.selectRow(existing.ingredient.quantity.whole ?? 0, inComponent: 0, animated: true)
            quantity.selectRow(existing.ingredient.quantity.fraction?.toInt() ?? 0, inComponent: 1, animated: true)
            break
        case .add(_):
            heading.text = "Add"
            synopsis.text = "Add ingredients"
            existingInfo.alpha = 0
            newInfo.alpha = 0
            currentWholeQuantity = 1
            quantity.selectRow(1, inComponent: 0, animated: true)
            quantity.selectRow(0, inComponent: 1, animated: true)
            break
        default: break
        }
        unit.addDoneButtonOnKeyboard()
        name.addDoneButtonOnKeyboard()
    }
}
