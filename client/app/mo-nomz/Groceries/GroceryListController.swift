//
//  GroceryListController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/9/21.
//

import MobileCoreServices
import UIKit

struct GroceryDragInfo {
    let toBuy: Bool
    let indexPath: IndexPath
}

class GroceryListController: UITableViewController, UITableViewDragDelegate, UITableViewDropDelegate {
    var toBuy: [ReadableGroceryItemWithId] = []
    var bought: [ReadableGroceryItemWithId] = []
    var onChange: (() -> Void)? = nil
    var mergeItems: (ReadableGroceryItemWithId, ReadableGroceryItemWithId)? = nil
    var editItem: ReadableGroceryItemWithId? = nil
    var collapsed: [Bool] = [false, true]
    
    let EMPTY = 0
    let REORDER_MERGE_TIP = 1
    let TO_BUY_HEADING = 2
    let TO_BUY = 3
    let BOUGHT_HEADING = 4
    let BOUGHT = 5
    
    private func hasData() -> Bool {
        return (toBuy.count + bought.count) > 0
    }
    
    func selectRow(_ row: Int) {
        let item = toBuy[row]
        let newItem = ReadableGroceryItem(name: item.item.name, quantity: item.item.quantity, unit: item.item.unit, active: false, order: item.item.order)
        Database.updateGrocery(grocery: ReadableGroceryItemWithId(item: newItem, id: item.id))
        onChange?()
    }
    
    func deselectRow(_ row: Int) {
        let item = bought[row]
        let newItem = ReadableGroceryItem(name: item.item.name, quantity: item.item.quantity, unit: item.item.unit, active: true, order: item.item.order)
        Database.updateGrocery(grocery: ReadableGroceryItemWithId(item: newItem, id: item.id))
        onChange?()
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
        case REORDER_MERGE_TIP:
            let handler = { (action: UIAlertAction) -> Void in
                User.setDidDismissReorderMergeTip()
                DispatchQueue.main.async {
                    self.tableView.reloadData()
                }
            }
            promptForConfirmation(title: "Dismiss this tip", message: "Drag items to reorder or merge", handler: handler)
            break
        case TO_BUY_HEADING:
            collapsed[0] = !collapsed[0]
            DispatchQueue.main.async {
                self.tableView.reloadData()
            }
            break
        case TO_BUY:
            editRow(item: toBuy[indexPath.row])
            break
        case BOUGHT_HEADING:
            collapsed[1] = !collapsed[1]
            DispatchQueue.main.async {
                self.tableView.reloadData()
            }
            break
        case BOUGHT:
            editRow(item: bought[indexPath.row])
            break
        default:
            break
        }
    }
    
    override func numberOfSections(in tableView: UITableView) -> Int {
        return 6
    }
    
    private func collapsedSection(_ section: Int) -> Int? {
        switch section {
        case TO_BUY: return 0
        case BOUGHT: return 1
        default: return nil
        }
    }

    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        if let c = collapsedSection(section) {
            if collapsed[c] { return 0 }
        }
        switch section {
        case EMPTY: return hasData() ? 0 : 1
        case REORDER_MERGE_TIP: return (hasData() && !User.dismissedReorderMergeTip()) ? 1 : 0
        case TO_BUY_HEADING: return hasData() ? 1 : 0
        case TO_BUY: return toBuy.count
        case BOUGHT_HEADING: return hasData() ? 1 : 0
        case BOUGHT: return bought.count
        default: return 0
        }
    }
    
    func deleteRow(_ id: UUID) {
        Database.deleteGrocery(id: id)
        onChange?()
    }
    
    func editRow(item: ReadableGroceryItemWithId) {
        editItem = item
        performSegue(withIdentifier: "editItem", sender: nil)
    }
    
    private func swipe(_ indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        let id: UUID
        switch indexPath.section {
        case TO_BUY:
            id = toBuy[indexPath.row].id
            break
        case BOUGHT:
            id = bought[indexPath.row].id
            break
        default: return nil
        }
        let action = UIContextualAction(style: .destructive, title: "Delete") { [weak self] (action, view, completionHandler) in
            self?.deleteRow(id)
            completionHandler(true)
        }
        action.backgroundColor = .systemRed
        return UISwipeActionsConfiguration(actions: [action])
    }
    
    override func tableView(_ tableView: UITableView, trailingSwipeActionsConfigurationForRowAt indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        return swipe(indexPath)
    }
    
    override func tableView(_ tableView: UITableView, leadingSwipeActionsConfigurationForRowAt indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        return swipe(indexPath)
    }

    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        switch indexPath.section {
        case EMPTY:
            let cell = tableView.dequeueReusableCell(withIdentifier: "emptyItem")!
            return cell
        case REORDER_MERGE_TIP:
            return tableView.dequeueReusableCell(withIdentifier: "reorderMergeTip")!
        case TO_BUY_HEADING:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! LabelButton
            let image = collapsed[0] ? UIImage(systemName: "chevron.forward.circle.fill") : UIImage(systemName: "chevron.down.circle.fill")
            cell.button.setImage(image, for: .normal)
            cell.label.text = "To buy (\(toBuy.count))"
            return cell
        case TO_BUY:
            let cell = tableView.dequeueReusableCell(withIdentifier: "toBuyListItem") as! LabelButton
            let item = toBuy[indexPath.row].item
            cell.tag = indexPath.row
            cell.label.text = item.render()
            cell.button.tag = indexPath.row
            cell.button.addTarget(self, action: #selector(didTapToBuy), for: .touchUpInside)
            return cell
        case BOUGHT_HEADING:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! LabelButton
            let image = collapsed[1] ? UIImage(systemName: "chevron.forward.circle.fill") : UIImage(systemName: "chevron.down.circle.fill")
            cell.button.setImage(image, for: .normal)
            cell.label.text = "Bought (\(bought.count))"
            return cell
        case BOUGHT:
            let cell = tableView.dequeueReusableCell(withIdentifier: "boughtListItem") as! LabelButton
            let item = bought[indexPath.row].item
            cell.tag = indexPath.row
            cell.label.text = item.render()
            cell.button.tag = indexPath.row
            cell.button.addTarget(self, action: #selector(didTapBought), for: .touchUpInside)
            return cell
        default: return UITableViewCell()
        }
    }
    
    func tableView(_ tableView: UITableView, itemsForBeginning session: UIDragSession, at indexPath: IndexPath) -> [UIDragItem] {
        let item: ReadableGroceryItemWithId
        let isToBuy: Bool
        switch indexPath.section {
        case TO_BUY:
            item = toBuy[indexPath.row]
            isToBuy = true
            break
        case BOUGHT:
            item = bought[indexPath.row]
            isToBuy = false
            break
        default:
            return []
        }
        do {
            session.localContext = GroceryDragInfo(toBuy: isToBuy, indexPath: indexPath)
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
        case (TO_BUY, true):
            if indexPath.row < toBuy.count {
                tableView.scrollToRow(at: indexPath, at: .none, animated: true)
            } else {
                tableView.scrollToRow(at: IndexPath(row: toBuy.count - 1, section: indexPath.section), at: .none, animated: true)
            }
            break
        case (BOUGHT, false):
            if indexPath.row < bought.count {
                tableView.scrollToRow(at: indexPath, at: .none, animated: true)
            } else {
                tableView.scrollToRow(at: IndexPath(row: bought.count - 1, section: indexPath.section), at: .none, animated: true)
            }
            break
        default: return cancel
        }
        if tableView.hasActiveDrag {
            return UITableViewDropProposal(operation: .move, intent: .automatic)
        }
        return cancel
    }

    func tableView(_ tableView: UITableView, performDropWith coordinator: UITableViewDropCoordinator) {
        guard let indexPath = coordinator.destinationIndexPath else { return }
        guard let info = coordinator.session.localDragSession?.localContext as? GroceryDragInfo else { return }
        let existing: ReadableGroceryItemWithId
        let newOrder: Int
        let isMerge = coordinator.proposal.intent == .insertIntoDestinationIndexPath
        switch (indexPath.section, isMerge) {
        case (TO_BUY, true):
            existing = toBuy[indexPath.row]
            newOrder = existing.item.order
            break
        case (TO_BUY, false):
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
        case (BOUGHT, true):
            existing = bought[tableView.cellForRow(at: indexPath)!.tag]
            newOrder = existing.item.order
            break
        case (BOUGHT, false):
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
                    if isMerge {
                        let run = { () -> Void in
                            self.mergeItems = (existing, new)
                            self.performSegue(withIdentifier: "mergeItems", sender: nil)
                        }
                        let runAndIgnore = { () -> Void in
                            User.setDidDismissMergeWarning()
                            run()
                        }
                        if !User.dismissedMergeWarning() {
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
                    } else {
                        let newItem = ReadableGroceryItem(name: new.item.name, quantity: new.item.quantity, unit: new.item.unit, active: existing.item.active, order: newOrder)
                        Database.updateGrocery(grocery: ReadableGroceryItemWithId(item: newItem, id: new.id))
                        self.onChange?()
                    }
                } catch {
                    print("Failed completing drag and drop \(error)")
                }
            }
        })
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? GroceryChangeController {
            vc.onChange = onChange
            switch segue.identifier {
            case "mergeItems":
                vc.change = .merge(mergeItems!.0, mergeItems!.1)
                break
            case "editItem":
                vc.change = .edit(editItem!)
                break
            default: break
            }
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        tableView.dragDelegate = self
        tableView.dropDelegate = self
        tableView.dragInteractionEnabled = true
    }
}
