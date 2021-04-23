//
//  RecipeAddController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/12/21.
//

import UIKit

class RecipeAddController: UIViewController, UIPickerViewDataSource, UIPickerViewDelegate {
    @IBOutlet weak var link: UITextField!
    @IBOutlet weak var name: UITextField!
    @IBOutlet weak var quantity: UIPickerView!
    @IBOutlet weak var unit: UITextField!
    @IBOutlet weak var toolbar: UIToolbar!
    var onChange: (() -> Void)?
    var currentWholeQuantity: Int? = nil
    var currentFractionQuantity: ReadableFraction? = nil

    @IBAction func didTapSave(_ sender: Any) {
        let completion = { () -> Void in
            DispatchQueue.main.async {
                self.dismiss(animated: true, completion: nil)
            }
            self.onChange?()
        }
        if (!(name.text?.isEmpty ?? true)) {
            addIngredientSingle(name: name.text!, quantity: ReadableQuantity(whole: currentWholeQuantity, fraction: currentFractionQuantity), unit: unit.text, completion: completion)
        } else if !(link.text?.isEmpty ?? true) {
            addRecipeLink(link: link.text!, completion: completion)
        }
    }
    
    @IBAction func didTapCancel(_ sender: Any) {
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
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
        link.becomeFirstResponder()
        link.addDoneButtonOnKeyboard()
        currentWholeQuantity = 1
        currentFractionQuantity = nil
        quantity.selectRow(1, inComponent: 0, animated: true)
        quantity.selectRow(0, inComponent: 1, animated: true)
        name.addDoneButtonOnKeyboard()
        unit.addDoneButtonOnKeyboard()
    }
}
