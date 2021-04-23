//
//  GroceryAddController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/23/21.
//

import UIKit

class GroceryAddController: UIViewController, UIPickerViewDataSource, UIPickerViewDelegate {
    var onChange: (() -> Void)? = nil
    var addItemVc: GroceryAddItemController? = nil
    var newWholeQuantity: Int? = nil
    var newFractionQuantity: ReadableFraction? = nil
    
    @IBOutlet weak var name: UITextField!
    @IBOutlet weak var unit: UITextField!
    @IBOutlet weak var quantity: UIPickerView!

    @IBAction func didTapCancel(_ sender: Any?) {
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
    }
    
    @IBAction func didTapAdd(_ sender: Any?) {
        if let newName = name.text, !newName.isEmpty {
            addItemVc?.items.append(ImportGrocerySingle(name: newName, quantity: ReadableQuantity(whole: newWholeQuantity, fraction: newFractionQuantity), unit: unit.text))
            DispatchQueue.main.async {
                self.resetInputs()
                self.addItemVc?.tableView.reloadData()
            }
        }
    }
    
    @IBAction func didTapSave(_ sender: Any?) {
        if let items = addItemVc?.items, !items.isEmpty {
            let completion = {
                DispatchQueue.main.async {
                    self.dismiss(animated: true, completion: nil)
                }
                self.onChange?()
            }
            addGroceryList(items: items, completion: completion)
        } else {
            didTapCancel(sender)
        }
    }
    
    func pickerView(_ pickerView: UIPickerView, didSelectRow row: Int, inComponent component: Int) {
        switch component {
        case 0:
            newWholeQuantity = row
            break
        default:
            newFractionQuantity = ReadableFraction.fromInt(x: row)
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
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? GroceryAddItemController, segue.identifier == "embedAddItems" {
            addItemVc = vc
        }
    }
    
    private func resetInputs() {
        unit.text = nil
        unit.becomeFirstResponder()
        unit.addDoneButtonOnKeyboard()
        name.text = nil
        name.addDoneButtonOnKeyboard()
        newWholeQuantity = 1
        newFractionQuantity = nil
        quantity.selectRow(1, inComponent: 0, animated: true)
        quantity.selectRow(0, inComponent: 1, animated: true)
    }
    
    override func viewDidLoad() {
        resetInputs()
    }
}
