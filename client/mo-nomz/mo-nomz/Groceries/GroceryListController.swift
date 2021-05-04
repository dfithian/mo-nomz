//
//  GroceryListController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/9/21.
//

import MobileCoreServices
import UIKit

class GroceryListController: UITableViewController, UITableViewDragDelegate, UITableViewDropDelegate {
    var toBuy: [ReadableGroceryItemWithId] = []
    var bought: [ReadableGroceryItemWithId] = []
    var onChange: (() -> Void)? = nil
    var mergeItems: (ReadableGroceryItem, ReadableGroceryItem, [Int])? = nil
    var editItem: ReadableGroceryItemWithId? = nil
    var collapsed: [Bool] = [false, true]
    
    private func hasData() -> Bool {
        return (toBuy.count + bought.count) > 0
    }
    
    func selectRow(_ row: Int) {
        let item = toBuy[row]
        let newItem = ReadableGroceryItem(name: item.item.name, quantity: item.item.quantity, unit: item.item.unit, active: false, order: item.item.order)
        updateGroceryItem(groceryItemId: item.id, groceryItem: newItem, completion: onChange)
    }
    
    func deselectRow(_ row: Int) {
        let item = bought[row]
        let newItem = ReadableGroceryItem(name: item.item.name, quantity: item.item.quantity, unit: item.item.unit, active: true, order: item.item.order)
        updateGroceryItem(groceryItemId: item.id, groceryItem: newItem, completion: onChange)
    }
    
    @objc func didTapToBuy(_ sender: Any?) {
        let b = sender as! UIButton
        selectRow(b.tag)
    }
    
    private func move(row: Int, up: Bool, active: Bool) {
        let order: Int
        let items = active ? toBuy : bought
        let existing = items[row]
        if up {
            guard row != 0 else { return }
            order = items[row - 1].item.order
        } else {
            guard row < items.count - 1 else { return }
            order = items[row + 1].item.order + 1
        }
        let new = ReadableGroceryItem(name: existing.item.name, quantity: existing.item.quantity, unit: existing.item.unit, active: existing.item.active, order: order)
        updateGroceryItem(groceryItemId: existing.id, groceryItem: new, completion: onChange)
    }
    
    @objc func didTapToBuyMoveUp(_ sender: Any?) {
        let b = sender as! UIButton
        move(row: b.tag, up: true, active: true)
    }
    
    @objc func didTapToBuyMoveDown(_ sender: Any?) {
        let b = sender as! UIButton
        move(row: b.tag, up: false, active: true)
    }
    
    @objc func didTapBoughtMoveUp(_ sender: Any?) {
        let b = sender as! UIButton
        move(row: b.tag, up: true, active: false)
    }
    
    @objc func didTapBoughtMoveDown(_ sender: Any?) {
        let b = sender as! UIButton
        move(row: b.tag, up: false, active: false)
    }
    
    @objc func didTapBought(_ sender: Any?) {
        let b = sender as! UIButton
        deselectRow(b.tag)
    }
    
    override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        switch indexPath.section {
        case 1:
            collapsed[0] = !collapsed[0]
            DispatchQueue.main.async {
                self.tableView.reloadData()
            }
            break
        case 2:
            editRow(item: toBuy[indexPath.row])
            break
        case 3:
            collapsed[1] = !collapsed[1]
            DispatchQueue.main.async {
                self.tableView.reloadData()
            }
            break
        case 4:
            editRow(item: bought[indexPath.row])
            break
        default:
            break
        }
    }
    
    override func numberOfSections(in tableView: UITableView) -> Int {
        return 5
    }
    
    private func collapsedSection(_ section: Int) -> Int? {
        switch section {
        case 2: return 0
        case 4: return 1
        default: return nil
        }
    }

    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        if let c = collapsedSection(section) {
            if collapsed[c] { return 0 }
        }
        switch section {
        case 0: return hasData() ? 0 : 1
        case 1: return hasData() ? 1 : 0
        case 2: return toBuy.count
        case 3: return hasData() ? 1 : 0
        case 4: return bought.count
        default: return 1
        }
    }
    
    func deleteRow(_ ids: [Int]) {
        deleteGroceryItems(groceryItemIds: ids, completion: onChange)
    }
    
    func editRow(item: ReadableGroceryItemWithId) {
        editItem = item
        self.performSegue(withIdentifier: "editItem", sender: nil)
    }
    
    override func tableView(_ tableView: UITableView, trailingSwipeActionsConfigurationForRowAt indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        let id: Int
        switch indexPath.section {
        case 2:
            id = toBuy[indexPath.row].id
            break
        case 4:
            id = bought[indexPath.row].id
            break
        default:
            return nil
        }
        let action = UIContextualAction(style: .destructive, title: "Delete") { [weak self] (action, view, completionHandler) in
            self?.deleteRow([id])
            completionHandler(true)
        }
        action.backgroundColor = .systemRed
        return UISwipeActionsConfiguration(actions: [action])
    }
    
    override func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        switch indexPath.section {
        case 1: return self.tableView.sectionHeaderHeight
        case 3: return self.tableView.sectionHeaderHeight
        default: return 44
        }
    }

    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        switch indexPath.section {
        case 0:
            let cell = tableView.dequeueReusableCell(withIdentifier: "emptyItem")!
            return cell
        case 1:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SectionHeader
            let image = collapsed[0] ? UIImage(systemName: "chevron.forward.circle.fill") : UIImage(systemName: "chevron.down.circle.fill")
            cell.indicator.setImage(image, for: .normal)
            cell.label.text = "To buy (\(toBuy.count))"
            return cell
        case 2:
            let cell = tableView.dequeueReusableCell(withIdentifier: "toBuyListItem") as! GroceryListItem
            let item = toBuy[indexPath.row].item
            cell.name.text = item.render()
            cell.select.tag = indexPath.row
            cell.select.addTarget(self, action: #selector(didTapToBuy), for: .touchUpInside)
            cell.moveUp.tag = indexPath.row
            cell.moveUp.addTarget(self, action: #selector(didTapToBuyMoveUp), for: .touchUpInside)
            cell.moveDown.tag = indexPath.row
            cell.moveDown.addTarget(self, action: #selector(didTapToBuyMoveDown), for: .touchUpInside)
            return cell
        case 3:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SectionHeader
            let image = collapsed[1] ? UIImage(systemName: "chevron.forward.circle.fill") : UIImage(systemName: "chevron.down.circle.fill")
            cell.indicator.setImage(image, for: .normal)
            cell.label.text = "Bought (\(bought.count))"
            return cell
        case 4:
            let cell = tableView.dequeueReusableCell(withIdentifier: "boughtListItem") as! GroceryListItem
            let item = bought[indexPath.row].item
            cell.name.text = item.render()
            cell.select.tag = indexPath.row
            cell.select.addTarget(self, action: #selector(didTapBought), for: .touchUpInside)
            cell.moveUp.tag = indexPath.row
            cell.moveUp.addTarget(self, action: #selector(didTapBoughtMoveUp), for: .touchUpInside)
            cell.moveDown.tag = indexPath.row
            cell.moveDown.addTarget(self, action: #selector(didTapBoughtMoveDown), for: .touchUpInside)
            return cell
        default:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SectionHeader
            return cell
        }
    }
    
    func tableView(_ tableView: UITableView, itemsForBeginning session: UIDragSession, at indexPath: IndexPath) -> [UIDragItem] {
        var item: ReadableGroceryItemWithId? = nil
        switch indexPath.section {
        case 2:
            item = toBuy[indexPath.row]
            break
        case 4:
            item = bought[indexPath.row]
            break
        default:
            break
        }
        do {
            if let i = item {
                let data = try JSONEncoder().encode(i)
                return [UIDragItem(itemProvider: NSItemProvider(item: data as NSData, typeIdentifier: kUTTypePlainText as String))]
            }
        } catch {
            print("Failed to initiate drag and drop \(error)")
        }
        return []
    }

    func tableView(_ tableView: UITableView, dropSessionDidUpdate session: UIDropSession, withDestinationIndexPath destinationIndexPath: IndexPath?) -> UITableViewDropProposal {
        var proposal = UITableViewDropProposal(operation: .cancel)
        guard let indexPath = destinationIndexPath else { return proposal }
        guard indexPath.section == 2 || indexPath.section == 4 else { return proposal }
        guard session.items.count == 1 else { return proposal }
        switch indexPath.section {
        case 2:
            if indexPath.row < toBuy.count {
                tableView.scrollToRow(at: indexPath, at: .none, animated: true)
            } else {
                tableView.scrollToRow(at: IndexPath(row: toBuy.count - 1, section: indexPath.section), at: .none, animated: true)
            }
            break
        case 4:
            if indexPath.row < bought.count {
                tableView.scrollToRow(at: indexPath, at: .none, animated: true)
            } else {
                tableView.scrollToRow(at: IndexPath(row: bought.count - 1, section: indexPath.section), at: .none, animated: true)
            }
            break
        default: break
        }
        if tableView.hasActiveDrag {
            proposal = UITableViewDropProposal(operation: .move, intent: .insertIntoDestinationIndexPath)
        }
        return proposal
    }

    func tableView(_ tableView: UITableView, performDropWith coordinator: UITableViewDropCoordinator) {
        guard let indexPath = coordinator.destinationIndexPath else { return }
        let existing: ReadableGroceryItemWithId
        switch indexPath.section {
        case 2:
            existing = toBuy[indexPath.row]
            break
        case 4:
            existing = bought[indexPath.row]
            break
        default:
            return
        }
        coordinator.session.loadObjects(ofClass: NSString.self, completion: { items in
            guard let strings = items as? [String] else { return }
            for string in strings {
                do {
                    let prefs = Persistence.loadPreferencess()
                    let new = try JSONDecoder().decode(ReadableGroceryItemWithId.self, from: string.data(using: .utf8)!)
                    let run = { () -> Void in
                        self.mergeItems = (existing.item, new.item, [existing.id + new.id])
                        self.performSegue(withIdentifier: "mergeItems", sender: nil)
                    }
                    let runAndIgnore = { () -> Void in
                        Persistence.setPreferences(Preferences(dismissedMergeWarning: true))
                        run()
                    }
                    if !prefs.dismissedMergeWarning {
                        self.promptForConfirmationThree(
                            title: "Warning",
                            message: "Merging items may result in unexpected behavior when deleting recipes. If you need to delete recipes, do that first.",
                            option1Button: "OK",
                            option1Handler: { _ in run() },
                            option2Button: "Ignore future warnings",
                            option2Handler: { _ in runAndIgnore() }
                        )
                    } else {
                        run()
                    }
                } catch {
                    print("Failed completing drag and drop \(error)")
                }
            }
        })
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? GroceryMergeController, segue.identifier == "mergeItems" {
            vc.existing = self.mergeItems!.0
            vc.new = self.mergeItems!.1
            vc.ids = self.mergeItems!.2
            vc.onChange = onChange
        }
        if let vc = segue.destination as? GroceryEditController, segue.identifier == "editItem" {
            vc.existing = self.editItem!
            vc.onChange = onChange
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        tableView.dragDelegate = self
        tableView.dropDelegate = self
        tableView.dragInteractionEnabled = true
        tableView.layer.cornerRadius = 5
    }
}
