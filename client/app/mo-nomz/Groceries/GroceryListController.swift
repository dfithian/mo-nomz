//
//  GroceryListController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/9/21.
//

import MobileCoreServices
import UIKit

enum GroceryDragType: Codable {
    case item(ReadableGroceryItemWithId)
    case group(GroceryGroupWithId)
    
    var isGroup: Bool {
        switch self {
        case .item(_): return false
        case .group(_): return true
        }
    }
}

enum GroceryListItem {
    case item(ReadableGroceryItemWithId)
    case group(GroceryGroupWithId)
    case uncategorized
    
    var isGroup: Bool {
        switch self {
        case .item(_): return false
        case .group(_): return true
        case .uncategorized: return true
        }
    }
    
    var isItem: Bool {
        switch self {
        case .item(_): return true
        case .group(_): return false
        case .uncategorized: return false
        }
    }
    
    var asItem: ReadableGroceryItemWithId? {
        switch self {
        case .item(let item): return item
        default: return nil
        }
    }
    
    var asGroup: GroceryGroupWithId? {
        switch self {
        case .item(let item): return item.item.group
        case .group(let group): return group
        case .uncategorized: return nil
        }
    }
    
    var asDraggable: GroceryDragType? {
        switch self {
        case .item(let item): return .item(item)
        case .group(let group): return .group(group)
        default: return nil
        }
    }
}

struct GroceryDragInfo {
    let isGroup: Bool
    let indexPath: IndexPath
}

class GroceryListController: UITableViewController, UITableViewDragDelegate, UITableViewDropDelegate, UITextViewDelegate {
    var toBuy: [GroceryListItem] = []
    var bought: [GroceryListItem] = []
    var toBuyCollapsed: Bool = false
    var boughtCollapsed: Bool = true

    let EMPTY = 0
    let REORDER_MERGE_TIP = 1
    let TO_BUY_HEADING = 2
    let NEW_GROUP = 3
    let TO_BUY = 4
    let BOUGHT_HEADING = 5
    let BOUGHT = 6
    
    private func hasData() -> Bool {
        return toBuy.filter({ $0.isItem }).count + bought.count > 0
    }
    
    private func toggleRow(_ item: ReadableGroceryItemWithId) {
        let newItem = ReadableGroceryItem(name: item.item.name, quantity: item.item.quantity, unit: item.item.unit, active: !item.item.active, order: item.item.order, group: item.item.group)
        Database.updateGrocery(grocery: ReadableGroceryItemWithId(item: newItem, id: item.id))
        reloadData()
    }
    
    @objc func didTapToBuy(_ sender: Any?) {
        let button = sender as! UIButton
        if let item = toBuy[button.tag].asItem {
            toggleRow(item)
        }
    }
    
    @objc func didTapBought(_ sender: Any?) {
        let button = sender as! UIButton
        if let item = bought[button.tag].asItem {
            toggleRow(item)
        }
    }
    
    @objc func didTapCreateGroup(_ sender: Any?) {
        newGroup()
    }
    
    private func deleteRow(_ id: UUID) {
        Database.deleteGrocery(id: id)
        reloadData()
    }
    
    private func deleteGroup(_ id: UUID) {
        Database.deleteGroup(id: id)
        reloadData()
    }
    
    private func newGroup() {
        let newGroup = GroceryGroupWithId(group: GroceryGroup(name: "", order: 0), id: UUID())
        Database.insertGroups(groups: [newGroup])
        toBuy.insert(.group(newGroup), at: 0)
        let indexPath = IndexPath(row: 0, section: TO_BUY)
        tableView.insertRows(at: [indexPath], with: .automatic)
        editGroup(indexPath)
    }
    
    private func editGroup(_ indexPath: IndexPath) {
        let cell = tableView.cellForRow(at: indexPath) as! LabelText
        cell.text_.becomeFirstResponder()
        cell.text_.alpha = 1
        cell.label.alpha = 0
    }
    
    func textViewDidEndEditing(_ textView: UITextView) {
        let group: GroceryGroupWithId
        switch toBuy[textView.tag] {
        case .group(let existing):
            group = existing
            break
        default: return
        }
        let cell = tableView.cellForRow(at: IndexPath(row: textView.tag, section: TO_BUY)) as! LabelText
        cell.text_.alpha = 0
        cell.label.alpha = 1
        cell.label.text = cell.text_.text
        if let newGroup = cell.text_.text?.trimmingCharacters(in: .whitespacesAndNewlines).nonEmpty() {
            let new = GroceryGroupWithId(group: GroceryGroup(name: newGroup, order: group.group.order), id: group.id)
            Database.updateGroup(group: new)
        } else {
            Database.deleteGroup(id: group.id)
        }
        reloadData()
    }

    override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        switch indexPath.section {
        case REORDER_MERGE_TIP:
            let handler = { (action: UIAlertAction) -> Void in
                User.setDidDismissReorderMergeTip()
                tableView.reloadData()
            }
            promptForConfirmation(title: "Dismiss this tip", message: "Drag items to reorder or merge", handler: handler)
            break
        case TO_BUY_HEADING:
            toBuyCollapsed = !toBuyCollapsed
            tableView.reloadData()
            break
        case NEW_GROUP:
            newGroup()
            break
        case TO_BUY:
            switch toBuy[indexPath.row] {
            case .item(let item):
                performSegue(withIdentifier: "editItem", sender: GroceryChange.edit(item))
                break
            case .group(_):
                editGroup(indexPath)
                break
            case .uncategorized: break
            }
            break
        case BOUGHT_HEADING:
            boughtCollapsed = !boughtCollapsed
            tableView.reloadData()
            break
        default: break
        }
    }

    override func numberOfSections(in tableView: UITableView) -> Int {
        return 7
    }

    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        switch section {
        case EMPTY: return hasData() ? 0 : 1
        case REORDER_MERGE_TIP: return (hasData() && !User.dismissedReorderMergeTip()) ? 1 : 0
        case TO_BUY_HEADING: return hasData() ? 1 : 0
        case NEW_GROUP: return hasData() ? 1 : 0
        case TO_BUY:
            if toBuyCollapsed || !hasData() {
                return 0
            }
            return toBuy.count
        case BOUGHT_HEADING: return hasData() ? 1 : 0
        case BOUGHT:
            if boughtCollapsed || !hasData() {
                return 0
            }
            return bought.count
        default: return 0
        }
    }

    private func swipe(_ indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        let itemAction = { (id: UUID) -> UISwipeActionsConfiguration? in
            let action = UIContextualAction(style: .destructive, title: "Delete") { (action, view, completionHandler) in
                self.deleteRow(id)
                completionHandler(true)
            }
            action.backgroundColor = .systemRed
            return UISwipeActionsConfiguration(actions: [action])
        }
        let groupAction = { (id: UUID) -> UISwipeActionsConfiguration? in
            let action = UIContextualAction(style: .destructive, title: "Delete") { (action, view, completionHandler) in
                self.deleteGroup(id)
                completionHandler(true)
            }
            action.backgroundColor = .systemRed
            return UISwipeActionsConfiguration(actions: [action])
        }
        switch indexPath.section {
        case TO_BUY:
            switch toBuy[indexPath.row] {
            case .item(let item): return itemAction(item.id)
            case .group(let group): return groupAction(group.id)
            case .uncategorized: return nil
            }
        case BOUGHT:
            switch bought[indexPath.row] {
            case .item(let item): return itemAction(item.id)
            case .group(let group): return groupAction(group.id)
            case .uncategorized: return nil
            }
        default: return nil
        }
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
            let imageName = toBuyCollapsed ? "chevron.forward.circle.fill" : "chevron.down.circle.fill"
            cell.label.text = "To buy (\(toBuy.filter({ $0.isItem }).count))"
            cell.button.setImage(UIImage(systemName: imageName), for: .normal)
            return cell
        case NEW_GROUP:
            let cell = tableView.dequeueReusableCell(withIdentifier: "newGroup") as! OneButton
            cell.button.addTarget(self, action: #selector(didTapCreateGroup), for: .touchUpInside)
            return cell
        case TO_BUY:
            switch toBuy[indexPath.row] {
            case .item(let item):
                let cell = tableView.dequeueReusableCell(withIdentifier: "toBuyListItem") as! LabelButton
                cell.label.text = item.item.render()
                cell.button.tag = indexPath.row
                cell.button.addTarget(self, action: #selector(didTapToBuy), for: .touchUpInside)
                return cell
            case .group(let group):
                let cell = tableView.dequeueReusableCell(withIdentifier: "groupHeader") as! LabelText
                cell.label.text = group.group.name
                cell.text_.text = group.group.name
                cell.text_.tag = indexPath.row
                cell.text_.addDoneButtonOnKeyboard()
                cell.text_.delegate = self
                return cell
            case .uncategorized:
                let cell = tableView.dequeueReusableCell(withIdentifier: "groupHeader") as! LabelText
                cell.label.text = "Uncategorized"
                return cell
            }
        case BOUGHT_HEADING:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! LabelButton
            let imageName = boughtCollapsed ? "chevron.forward.circle.fill" : "chevron.down.circle.fill"
            cell.label.text = "Bought (\(bought.filter({ $0.isItem }).count))"
            cell.button.setImage(UIImage(systemName: imageName), for: .normal)
            return cell
        case BOUGHT:
            switch bought[indexPath.row] {
            case .item(let item):
                let cell = tableView.dequeueReusableCell(withIdentifier: "boughtListItem") as! LabelButton
                cell.label.text = item.item.render()
                cell.button.tag = indexPath.row
                cell.button.addTarget(self, action: #selector(didTapBought), for: .touchUpInside)
                return cell
            case .group(let group):
                let cell = tableView.dequeueReusableCell(withIdentifier: "boughtGroupHeader") as! OneLabel
                cell.label.text = group.group.name
                return cell
            case .uncategorized:
                let cell = tableView.dequeueReusableCell(withIdentifier: "boughtGroupHeader") as! OneLabel
                cell.label.text = "Uncategorized"
                return cell
            }
        default: return UITableViewCell()
        }
    }

    func tableView(_ tableView: UITableView, itemsForBeginning session: UIDragSession, at indexPath: IndexPath) -> [UIDragItem] {
        let draggable: GroceryDragType
        switch indexPath.section {
        case TO_BUY:
            if let draggable_ = toBuy[indexPath.row].asDraggable {
                draggable = draggable_
            } else {
                return []
            }
            break
        default: return []
        }
        do {
            session.localContext = GroceryDragInfo(isGroup: draggable.isGroup, indexPath: indexPath)
            let data = try JSONEncoder().encode(draggable)
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
        let sanitizedIndexPath: IndexPath
        switch indexPath.section {
        case TO_BUY:
            if indexPath.row < toBuy.count {
                sanitizedIndexPath = indexPath
            } else {
                sanitizedIndexPath = IndexPath(row: indexPath.row - 1, section: indexPath.section)
            }
            break
        default: return cancel
        }
        tableView.scrollToRow(at: sanitizedIndexPath, at: .none, animated: true)
        if tableView.hasActiveDrag {
            if toBuy[sanitizedIndexPath.row].isGroup {
                return UITableViewDropProposal(operation: .move, intent: .insertAtDestinationIndexPath)
            }
            if info.isGroup {
                return cancel
            }
            return UITableViewDropProposal(operation: .move, intent: .automatic)
        }
        return cancel
    }
    
    private func reorder<T>(source: Int, destination: Int, items: [T], extract: ((T) -> Int)) -> Int {
        if source < destination {
            return extract(items[destination]) + 1
        } else if source == 0 {
            return extract(items[destination])
        } else {
            return extract(items[destination - 1]) + 1
        }
    }

    func tableView(_ tableView: UITableView, performDropWith coordinator: UITableViewDropCoordinator) {
        guard let indexPath = coordinator.destinationIndexPath else { return }
        guard let info = coordinator.session.localDragSession?.localContext as? GroceryDragInfo else { return }
        let isMerge = coordinator.proposal.intent == .insertIntoDestinationIndexPath
        coordinator.session.loadObjects(ofClass: NSString.self, completion: { items in
            guard let strings = items as? [String] else { return }
            for string in strings {
                do {
                    let new = try JSONDecoder().decode(GroceryDragType.self, from: string.data(using: .utf8)!)
                    switch (new, isMerge) {
                    case (.item(let item), true):
                        guard let existing = self.toBuy[indexPath.row].asItem else { return }
                        let run = { () -> Void in
                            self.performSegue(withIdentifier: "mergeItems", sender: GroceryChange.merge(existing, item))
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
                        break
                    case (.item(let item), false):
                        let newActive: Bool
                        let newOrder: Int
                        let newGroup: GroceryGroupWithId?
                        switch indexPath.section {
                        case self.TO_BUY:
                            newActive = true
                            newOrder = self.reorder(source: info.indexPath.row, destination: indexPath.row, items: self.toBuy, extract: {
                                switch $0 {
                                case .item(let item): return item.item.order
                                case .group(_): return 0
                                case .uncategorized: return 0
                                }
                            })
                            newGroup = self.toBuy[indexPath.row].asGroup
                            break
                        default: return
                        }
                        let newItem = ReadableGroceryItem(name: item.item.name, quantity: item.item.quantity, unit: item.item.unit, active: newActive, order: newOrder, group: newGroup)
                        Database.updateGrocery(grocery: ReadableGroceryItemWithId(item: newItem, id: item.id))
                        self.reloadData()
                        break
                    case (.group(let group), false):
                        let newOrder = self.reorder(source: info.indexPath.row, destination: indexPath.row, items: self.toBuy, extract: {
                            switch $0 {
                            case .item(let item): return item.item.group?.group.order ?? Database.selectMaxGroupOrder()
                            case .group(let group): return group.group.order
                            case .uncategorized: return Database.selectMaxGroupOrder()
                            }
                        })
                        Database.updateGroup(group: GroceryGroupWithId(group: GroceryGroup(name: group.group.name, order: newOrder), id: group.id))
                        self.reloadData()
                        break
                    default:
                        break
                    }
                } catch {
                    print("Failed completing drag and drop \(error)")
                }
            }
        })
    }

    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? GroceryChangeController, ["editItem", "mergeItems"].contains(segue.identifier), let change = sender as? GroceryChange {
            vc.onChange = reloadData
            vc.change = change
        }
    }
    
    private func loadData() {
        toBuy = []
        bought = []
        let groups = Database.selectGroups()
        let items = Database.selectGroceries()
        for group in groups {
            toBuy.append(.group(group))
            toBuy.append(contentsOf: items.filter({
                $0.item.group?.id == group.id && $0.item.active
            }).map({ .item($0) }))
            bought.append(.group(group))
            bought.append(contentsOf: items.filter({
                $0.item.group?.id == group.id && !$0.item.active
            }).map({ .item($0) }))
        }
        toBuy.append(.uncategorized)
        toBuy.append(contentsOf: items.filter({
            $0.item.group == nil && $0.item.active
        }).map({ .item($0) }))
        bought.append(.uncategorized)
        bought.append(contentsOf: items.filter({
            $0.item.group == nil && !$0.item.active
        }).map({ .item($0) }))
    }
    
    func reloadData() {
        loadData()
        tableView.reloadData()
    }

    override func viewDidLoad() {
        super.viewDidLoad()
        loadData()
        tableView.dragDelegate = self
        tableView.dropDelegate = self
        tableView.dragInteractionEnabled = true
    }
}
