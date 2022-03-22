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

class GroceryListController: UITableViewController, UITableViewDragDelegate, UITableViewDropDelegate, UITextFieldDelegate {
    var toBuy: [GroceryListItem] = []
    var bought: [GroceryListItem] = []
    var toBuyCollapsed: Bool = false
    var boughtCollapsed: Bool = true

    let EMPTY = 0
    let TO_BUY_HEADING = 1
    let NEW_GROUP = 2
    let TO_BUY = 3
    let BOUGHT_HEADING = 4
    let BOUGHT = 5
    
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
        let cell = tableView.cellForRow(at: indexPath) as! OneTextField
        cell.text_.becomeFirstResponder()
    }
    
    func textFieldDidEndEditing(_ textField: UITextField) {
        let group: GroceryGroupWithId
        switch toBuy[textField.tag] {
        case .group(let existing):
            group = existing
            break
        default: return
        }
        let cell = tableView.cellForRow(at: IndexPath(row: textField.tag, section: TO_BUY)) as! OneTextField
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
        return 6
    }

    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        switch section {
        case EMPTY: return hasData() ? 0 : 1
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
    
    override func tableView(_ tableView: UITableView, contextMenuConfigurationForRowAt indexPath: IndexPath, point: CGPoint) -> UIContextMenuConfiguration? {
        let groups = Database.selectGroups()
        let itemConfig = { (item: ReadableGroceryItemWithId) -> UIContextMenuConfiguration in
            let groupActions = groups.map({ (group) in
                UIAction(title: group.group.name, state: item.item.group?.id == group.id ? .on : .off, handler: { _ in
                    if item.item.group?.id != group.id {
                        Database.updateGrocery(grocery: ReadableGroceryItemWithId(item: ReadableGroceryItem(name: item.item.name, quantity: item.item.quantity, unit: item.item.unit, active: item.item.active, order: 0, group: group), id: item.id))
                        self.reloadData()
                    }
                })
            })
            let removeGroup = UIAction(title: "Remove group", image: UIImage(systemName: "xmark"), attributes: .destructive, handler: { _ in
                if item.item.group == nil {
                    Database.updateGrocery(grocery: ReadableGroceryItemWithId(item: ReadableGroceryItem(name: item.item.name, quantity: item.item.quantity, unit: item.item.unit, active: item.item.active, order: 0, group: nil), id: item.id))
                    self.reloadData()
                }
            })
            let groupMenu = UIMenu(title: "Manage Group", children: groupActions + [removeGroup])
            var children: [UIMenuElement] = []
            if item.item.active {
                children.append(groupMenu)
            }
            children.append(contentsOf: [
                UIAction(title: "Edit item", image: UIImage(systemName: "pencil"), handler: { _ in
                    self.performSegue(withIdentifier: "editItem", sender: GroceryChange.edit(item))
                }),
                UIAction(title: "Delete item", image: UIImage(systemName: "xmark"), attributes: .destructive, handler: { _ in
                    self.promptForConfirmation(title: "Delete item", message: "Are you sure you want to delete?", handler: { _ in
                        Database.deleteGrocery(id: item.id)
                        self.reloadData()
                    })
                })
            ])
            return UIContextMenuConfiguration(identifier: nil, previewProvider: nil, actionProvider: { _ in UIMenu(children: children) })
        }
        let groupConfig = { (group: GroceryGroupWithId) -> UIContextMenuConfiguration in
            return UIContextMenuConfiguration(identifier: nil, previewProvider: nil, actionProvider: { _ in UIMenu(children: [
                UIAction(title: "Delete group", image: UIImage(systemName: "xmark"), attributes: .destructive, handler: { _ in
                    self.promptForConfirmation(title: "Delete group", message: "Are you sure you want to delete?", handler: { _ in
                        Database.deleteGroup(id: group.id)
                        self.reloadData()
                    })
                })
            ]) })
        }
        switch indexPath.section {
        case TO_BUY:
            switch toBuy[indexPath.row] {
            case .item(let item): return itemConfig(item)
            case .group(let group): return groupConfig(group)
            default: return nil
            }
        case BOUGHT:
            switch bought[indexPath.row] {
            case .item(let item): return itemConfig(item)
            default: return nil
            }
        default: return nil
        }
    }

    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        switch indexPath.section {
        case EMPTY:
            let cell = tableView.dequeueReusableCell(withIdentifier: "emptyItem")!
            return cell
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
                let cell = tableView.dequeueReusableCell(withIdentifier: "groupHeader") as! OneTextField
                cell.text_.text = group.group.name
                cell.text_.tag = indexPath.row
                cell.text_.addDoneButtonOnKeyboard()
                cell.text_.delegate = self
                return cell
            case .uncategorized:
                let cell = tableView.dequeueReusableCell(withIdentifier: "groupHeader") as! OneTextField
                cell.text_.isUserInteractionEnabled = false
                cell.text_.text = "Uncategorized"
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
    
    private func reorderedElement<T>(source: Int, destination: Int, items: [T]) -> (T, Int) {
        if source < destination {
            return (items[destination], 1)
        } else if source == 0 {
            return (items[destination], 0)
        } else if destination == 0 {
            return (items[destination], 0)
        } else {
            return (items[destination - 1], 1)
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
                            let (drop, addOrder) = self.reorderedElement(source: info.indexPath.row, destination: indexPath.row, items: self.toBuy)
                            switch drop {
                            case .item(let i):
                                newOrder = i.item.order + addOrder
                                newGroup = i.item.group
                            case .group(let g):
                                newOrder = 0
                                newGroup = g
                            case .uncategorized:
                                newOrder = 0
                                newGroup = nil
                            }
                            break
                        default: return
                        }
                        let newItem = ReadableGroceryItem(name: item.item.name, quantity: item.item.quantity, unit: item.item.unit, active: newActive, order: newOrder, group: newGroup)
                        Database.updateGrocery(grocery: ReadableGroceryItemWithId(item: newItem, id: item.id))
                        self.reloadData()
                        break
                    case (.group(let group), false):
                        let newOrder: Int
                        let (drop, addOrder) = self.reorderedElement(source: info.indexPath.row, destination: indexPath.row, items: self.toBuy)
                        switch drop {
                        case .item(let i):
                            newOrder = i.item.group.map({ $0.group.order + addOrder }) ?? Database.selectMaxGroupOrder()
                            break
                        case .group(let g):
                            newOrder = g.group.order + addOrder
                            break
                        case .uncategorized:
                            newOrder = Database.selectMaxGroupOrder()
                            break
                        }
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
    
    override func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        let defaultHeadingHeight = 44.5
        let groupHeadingHeight = 32.0
        switch indexPath.section {
        case TO_BUY:
            switch toBuy[indexPath.row] {
            case .group(_): return groupHeadingHeight
            case .uncategorized: return groupHeadingHeight
            default: break
            }
        case BOUGHT:
            switch bought[indexPath.row] {
            case .group(_): return groupHeadingHeight
            case .uncategorized: return groupHeadingHeight
            default: break
            }
        case NEW_GROUP: return groupHeadingHeight
        default: break
        }
        return defaultHeadingHeight
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
