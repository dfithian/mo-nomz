//
//  GroceryListController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/9/21.
//

import MobileCoreServices
import UIKit

enum DragAndDropType {
    case merge
    case reorder
}

struct GroceryDragInfo {
    let type: DragAndDropType
    let toBuy: Bool
    let indexPath: IndexPath
}

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
        performSegue(withIdentifier: "editItem", sender: nil)
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
    
    override func tableView(_ tableView: UITableView, leadingSwipeActionsConfigurationForRowAt indexPath: IndexPath) -> UISwipeActionsConfiguration? {
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
            cell.tag = indexPath.row
            cell.name.text = item.render()
            cell.select.tag = indexPath.row
            cell.select.addTarget(self, action: #selector(didTapToBuy), for: .touchUpInside)
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
            cell.tag = indexPath.row
            cell.name.text = item.render()
            cell.select.tag = indexPath.row
            cell.select.addTarget(self, action: #selector(didTapBought), for: .touchUpInside)
            return cell
        default:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SectionHeader
            return cell
        }
    }
    
    func tableView(_ tableView: UITableView, itemsForBeginning session: UIDragSession, at indexPath: IndexPath) -> [UIDragItem] {
        let item: ReadableGroceryItemWithId
        let isToBuy: Bool
        var type: DragAndDropType = .merge
        switch indexPath.section {
        case 2:
            item = toBuy[indexPath.row]
            isToBuy = true
            break
        case 4:
            item = bought[indexPath.row]
            isToBuy = false
            break
        default:
            return []
        }
        if let cell = tableView.cellForRow(at: indexPath) as? GroceryListItem {
            let loc = session.location(in: cell)
            if loc.x >= (cell.move.frame.origin.x - 10) && loc.x <= cell.move.frame.origin.x + cell.move.frame.width {
                type = .reorder
            }
        }
        do {
            session.localContext = GroceryDragInfo(type: type, toBuy: isToBuy, indexPath: indexPath)
            let data = try JSONEncoder().encode(item)
            return [UIDragItem(itemProvider: NSItemProvider(item: data as NSData, typeIdentifier: kUTTypePlainText as String))]
        } catch {
            print("Failed to initiate drag and drop \(error)")
        }
        return []
    }

    func tableView(_ tableView: UITableView, dropSessionDidUpdate session: UIDropSession, withDestinationIndexPath destinationIndexPath: IndexPath?) -> UITableViewDropProposal {
        let cancel = UITableViewDropProposal(operation: .cancel)
        guard let info = session.localDragSession?.localContext as? GroceryDragInfo else { return cancel }
        guard let indexPath = destinationIndexPath else { return cancel }
        guard session.items.count == 1 else { return cancel }
        switch (indexPath.section, info.toBuy) {
        case (2, true):
            if indexPath.row < toBuy.count {
                tableView.scrollToRow(at: indexPath, at: .none, animated: true)
            } else {
                tableView.scrollToRow(at: IndexPath(row: toBuy.count - 1, section: indexPath.section), at: .none, animated: true)
            }
            break
        case (4, false):
            if indexPath.row < bought.count {
                tableView.scrollToRow(at: indexPath, at: .none, animated: true)
            } else {
                tableView.scrollToRow(at: IndexPath(row: bought.count - 1, section: indexPath.section), at: .none, animated: true)
            }
            break
        default: return cancel
        }
        if tableView.hasActiveDrag {
            switch info.type {
            case .reorder: return UITableViewDropProposal(operation: .move, intent: .insertAtDestinationIndexPath)
            case .merge: return UITableViewDropProposal(operation: .move, intent: .insertIntoDestinationIndexPath)
            }
        }
        return cancel
    }

    func tableView(_ tableView: UITableView, performDropWith coordinator: UITableViewDropCoordinator) {
        guard let indexPath = coordinator.destinationIndexPath else { return }
        guard let info = coordinator.session.localDragSession?.localContext as? GroceryDragInfo else { return }
        let existing: ReadableGroceryItemWithId
        let newOrder: Int
        switch (indexPath.section, info.type) {
        case (2, .merge):
            existing = toBuy[indexPath.row]
            newOrder = existing.item.order
            break
        case (2, .reorder):
            if info.indexPath.row < indexPath.row {
                existing = toBuy[indexPath.row]
                newOrder = existing.item.order + 1
            } else if indexPath.row == 0 {
                existing = toBuy[indexPath.row]
                newOrder = existing.item.order
            } else {
                existing = toBuy[indexPath.row - 1]
                newOrder = existing.item.order + 1
            }
            break
        case (4, .merge):
            existing = bought[tableView.cellForRow(at: indexPath)!.tag]
            newOrder = existing.item.order
            break
        case (4, .reorder):
            if info.indexPath.row < indexPath.row {
                existing = bought[indexPath.row]
                newOrder = existing.item.order + 1
            } else if indexPath.row == 0 {
                existing = bought[indexPath.row]
                newOrder = existing.item.order
            } else {
                existing = bought[indexPath.row - 1]
                newOrder = existing.item.order + 1
            }
            break
        default:
            return
        }
        coordinator.session.loadObjects(ofClass: NSString.self, completion: { items in
            guard let strings = items as? [String] else { return }
            for string in strings {
                do {
                    let new = try JSONDecoder().decode(ReadableGroceryItemWithId.self, from: string.data(using: .utf8)!)
                    switch info.type {
                    case .reorder:
                        self.updateGroceryItem(groceryItemId: new.id, groceryItem: ReadableGroceryItem(name: new.item.name, quantity: new.item.quantity, unit: new.item.unit, active: existing.item.active, order: newOrder), completion: self.onChange)
                        break
                    case .merge:
                        let prefs = Persistence.loadPreferencess()
                        let run = { () -> Void in
                            self.mergeItems = (existing.item, new.item, [existing.id, new.id])
                            self.performSegue(withIdentifier: "mergeItems", sender: nil)
                        }
                        let runAndIgnore = { () -> Void in
                            Persistence.setPreferences(Preferences(dismissedMergeWarning: true, dismissedIngredientMergeWarning: prefs.dismissedIngredientMergeWarning))
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
                        break
                    }
                } catch {
                    print("Failed completing drag and drop \(error)")
                }
            }
        })
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? GroceryMergeController, segue.identifier == "mergeItems" {
            vc.existing = mergeItems!.0
            vc.new = mergeItems!.1
            vc.ids = mergeItems!.2
            vc.onChange = onChange
        }
        if let vc = segue.destination as? GroceryEditController, segue.identifier == "editItem" {
            vc.existing = editItem!
            vc.onChange = onChange
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        tableView.dragDelegate = self
        tableView.dropDelegate = self
        tableView.dragInteractionEnabled = true
    }
}
