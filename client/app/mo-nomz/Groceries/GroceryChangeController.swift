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

class GroceryChangeController: SimpleController, UIPickerViewDataSource, UIPickerViewDelegate {
    var change: GroceryChange? = nil
    var onChange: (() -> Void)? = nil
    var currentWholeQuantity: Int? = nil
    var currentFractionQuantity: ReadableFraction? = nil
    var currentGroup: GroceryGroupWithId? = nil
    
    @IBOutlet weak var heading: UILabel!
    @IBOutlet weak var synopsis: UILabel!
    @IBOutlet weak var quantity: UIPickerView!
    @IBOutlet weak var unit: UITextField!
    @IBOutlet weak var name: UITextField!
    @IBOutlet weak var existingInfo: UILabel!
    @IBOutlet weak var newInfo: UILabel!
    @IBOutlet weak var group: UIButton!
    
    let WHOLE = 0
    let FRACTION = 1
    
    @IBAction func didTapSave(_ sender: Any?) {
        switch change {
        case .merge(let existing, let new):
            let newOrder = existing.item.group?.id != currentGroup?.id ? 0 : existing.item.order
            let item = ReadableGroceryItemWithId(item: ReadableGroceryItem(name: name.text!, quantity: ReadableQuantity(whole: currentWholeQuantity, fraction: currentFractionQuantity), unit: unit.text?.nonEmpty(), active: existing.item.active, order: newOrder, group: currentGroup), id: UUID())
            Database.mergeGroceries(ids: [existing.id, new.id], grocery: item)
            break
        case .edit(let existing):
            let newOrder = existing.item.group?.id != currentGroup?.id ? 0 : existing.item.order
            let item = ReadableGroceryItem(name: name.text!, quantity: ReadableQuantity(whole: currentWholeQuantity, fraction: currentFractionQuantity), unit: unit.text?.nonEmpty(), active: existing.item.active, order: newOrder, group: currentGroup)
            Database.updateGrocery(grocery: ReadableGroceryItemWithId(item: item, id: existing.id))
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
        case WHOLE:
            currentWholeQuantity = row
            break
        case FRACTION:
            currentFractionQuantity = ReadableFraction.fromInt(x: row)
            break
        default: break
        }
    }
    
    func numberOfComponents(in pickerView: UIPickerView) -> Int {
        return 2
    }
    
    func pickerView(_ pickerView: UIPickerView, numberOfRowsInComponent component: Int) -> Int {
        switch component {
        case WHOLE: return 100
        case FRACTION: return 6
        default: return 0
        }
    }
    
    func pickerView(_ pickerView: UIPickerView, titleForRow row: Int, forComponent component: Int) -> String? {
        switch component {
        case WHOLE: return String(row)
        case FRACTION: return ReadableFraction.fromInt(x: row)?.render() ?? "0"
        default: return nil
        }
    }
    
    private func setupGroup() {
        let groups = Database.selectGroups()
        let groupActions = groups.map({ (group) in
            UIAction(title: group.group.name, handler: { _ in
                self.currentGroup = group
                self.group.setTitle(group.group.name, for: .normal)
            })
        })
        let clearGroup = UIAction(title: "Remove", image: UIImage(systemName: "xmark"), attributes: .destructive, handler: { _ in
            self.currentGroup = nil
            self.group.setTitle("Select group", for: .normal)
        })
        let addGroup = UIAction(title: "Add", image: UIImage(systemName: "plus"), handler: { _ in
            self.promptGetInput(title: "Add group", completion: { (new) in
                let order = groups.map({ $0.group.order }).max() ?? 0
                let group = GroceryGroupWithId(group: GroceryGroup(name: new, order: order), id: UUID())
                Database.insertGroups(groups: [group])
                self.currentGroup = group
                self.group.setTitle(new, for: .normal)
        })})
        group.setTitle(currentGroup?.group.name ?? "Select group", for: .normal)
        group.menu = UIMenu(title: "Select group", children: groupActions + [clearGroup] + [addGroup])
        group.showsMenuAsPrimaryAction = true
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
            currentGroup = existing.item.group
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
            currentGroup = existing.item.group
            break
        default: break
        }
        setupGroup()
        unit.addDoneButtonOnKeyboard()
        name.addDoneButtonOnKeyboard()
    }
}
