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

class GroceryGroupCollapsed {
    private var collapsed: [UUID:Bool] = [:]
    private var uncategorized: Bool = false
    
    func isGroupCollapsed(_ id: UUID?) -> Bool {
        switch id {
        case .some(let x):
            if let c = collapsed[x] {
                return c
            } else {
                collapsed[x] = true
                return true
            }
        case .none: return uncategorized
        }
    }
    
    func toggleGroupCollapsed(_ id: UUID?) {
        switch id {
        case .some(let x):
            collapsed[x] = !isGroupCollapsed(x)
            break
        case .none:
            uncategorized = !uncategorized
            break
        }
    }
    
    func collapseAll() {
        for id in collapsed.keys {
            collapsed[id] = true
        }
        uncategorized = true
    }
    
    func expandAll() {
        for id in collapsed.keys {
            collapsed[id] = false
        }
        uncategorized = false
    }
}

class GroceryListItems {
    private var which: [GroceryListItem]
    private var count: GroceryCounts
    private var collapsed: GroceryGroupCollapsed
    
    private class GroceryCounts {
        private var visibleItems: Int = 0
        private var allItems: Int = 0
        private var byGroup: [UUID:Int] = [:]
        private var byUncategorized: Int = 0
        
        func visibleItem() {
            self.visibleItems += 1
        }
        
        func allItem() {
            self.allItems += 1
        }
        
        func group(_ id: UUID?) {
            if let i = id {
                if let n = byGroup[i] {
                    byGroup[i] = n + 1
                } else {
                    byGroup[i] = 1
                }
            } else {
                byUncategorized += 1
            }
        }
        
        func getVisibleItems() -> Int {
            return self.visibleItems
        }
        
        func getAllItems() -> Int {
            return self.allItems
        }
        
        func getByGroup(_ id: UUID?) -> Int {
            if let i = id {
                return self.byGroup[i] ?? 0
            } else {
                return self.byUncategorized
            }
        }
    }
    
    required init(_ which: [GroceryListItem], _ collapsed: GroceryGroupCollapsed) {
        self.which = which
        self.collapsed = collapsed
        self.count = GroceryCounts()
        self.invalidateCount()
    }
    
    private func effect<T>(nth: ((GroceryCounts) -> Bool), extract: ((GroceryCounts, GroceryListItem?) -> T)) -> T {
        let count = GroceryCounts()
        var isCollapsed = false
        for thing in which {
            if nth(count) && !(thing.isItem && isCollapsed) {
                return extract(count, thing)
            }
            switch thing {
            case .item(let item):
                if !isCollapsed {
                    count.visibleItem()
                }
                count.allItem()
                count.group(item.item.group?.id)
                break
            case .group(let group):
                count.visibleItem()
                isCollapsed = collapsed.isGroupCollapsed(group.id)
                break
            case .uncategorized:
                count.visibleItem()
                isCollapsed = collapsed.isGroupCollapsed(nil)
                break
            }
        }
        return extract(count, nil)
    }
    
    func invalidateCount() {
        self.count = effect(
            nth: { _ in false },
            extract: { (count, _) in count }
        )
    }
    
    func getVisibleCount() -> Int {
        return count.getVisibleItems()
    }
    
    func getAllItemCount() -> Int {
        return count.getAllItems()
    }
    
    func getGroupCount(_ id: UUID?) -> Int {
        return count.getByGroup(id)
    }
    
    func getItem(_ index: Int) -> GroceryListItem? {
        return effect(
            nth: { $0.getVisibleItems() == index },
            extract: { (_, item) in item }
        )
    }
    
    func reordered(source: Int, destination: Int) -> (GroceryListItem?, Int) {
        if source < destination {
            return (getItem(destination), 1)
        } else if source == 0 {
            return (getItem(destination), 0)
        } else if destination == 0 {
            return (getItem(destination), 0)
        } else {
            return (getItem(destination - 1), 1)
        }
    }
}

class GroceryListController: UITableViewController, UITableViewDragDelegate, UITableViewDropDelegate {
    var toBuyGroupCollapsed: GroceryGroupCollapsed = GroceryGroupCollapsed()
    var boughtGroupCollapsed: GroceryGroupCollapsed = GroceryGroupCollapsed()
    var toBuy: GroceryListItems = GroceryListItems([], GroceryGroupCollapsed())
    var bought: GroceryListItems = GroceryListItems([], GroceryGroupCollapsed())
    var toBuyCollapsed: Bool = false
    var boughtCollapsed: Bool = true

    let TO_BUY_HEADING = 0
    let TO_BUY = 1
    let BOUGHT_HEADING = 2
    let BOUGHT = 3
    
    private func toggleRow(_ item: ReadableGroceryItemWithId) {
        let newItem = ReadableGroceryItem(name: item.item.name, quantity: item.item.quantity, unit: item.item.unit, active: !item.item.active, order: item.item.order, group: item.item.group)
        Database.updateGrocery(grocery: ReadableGroceryItemWithId(item: newItem, id: item.id))
        reloadData()
    }
    
    private func toggleSection(_ which: GroceryListItems, _ collapsed: GroceryGroupCollapsed, indexPath: IndexPath) {
        let id: UUID?
        switch which.getItem(indexPath.row) {
        case .item(let item):
            performSegue(withIdentifier: "editItem", sender: GroceryChange.edit(item))
            return
        case .group(let group):
            id = group.id
            break
        case .uncategorized:
            id = nil
            break
        default: return
        }
        collapsed.toggleGroupCollapsed(id)
        which.invalidateCount()
        let num = which.getGroupCount(id)
        DispatchQueue.main.async {
            if num > 0 {
                let reloads = Array(1...num).map({ IndexPath(row: indexPath.row + $0, section: indexPath.section) })
                if collapsed.isGroupCollapsed(id) {
                    self.tableView.deleteRows(at: reloads, with: .automatic)
                } else {
                    self.tableView.insertRows(at: reloads, with: .automatic)
                }
            }
            self.tableView.reloadRows(at: [indexPath], with: .automatic)
        }
    }
    
    @objc func didTapToBuy(_ sender: Any?) {
        let button = sender as! UIButton
        if let item = toBuy.getItem(button.tag)?.asItem {
            toggleRow(item)
        }
    }
    
    @objc func didTapBought(_ sender: Any?) {
        let button = sender as! UIButton
        if let item = bought.getItem(button.tag)?.asItem {
            toggleRow(item)
        }
    }
    
    private func deleteItem(_ id: UUID) {
        Database.deleteGrocery(id: id)
        reloadData()
    }
    
    private func deleteGroup(_ id: UUID) {
        Database.deleteGroup(id: id)
        reloadData()
    }

    override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        switch indexPath.section {
        case TO_BUY_HEADING:
            toBuyCollapsed = !toBuyCollapsed
            DispatchQueue.main.async {
                self.tableView.reloadSections(IndexSet(integer: self.TO_BUY), with: .automatic)
                self.tableView.reloadRows(at: [indexPath], with: .automatic)
            }
            break
        case TO_BUY:
            toggleSection(toBuy, toBuyGroupCollapsed, indexPath: indexPath)
            break
        case BOUGHT_HEADING:
            boughtCollapsed = !boughtCollapsed
            DispatchQueue.main.async {
                self.tableView.reloadSections(IndexSet(integer: self.BOUGHT), with: .automatic)
                self.tableView.reloadRows(at: [indexPath], with: .automatic)
            }
            break
        case BOUGHT:
            toggleSection(bought, boughtGroupCollapsed, indexPath: indexPath)
            break
        default: break
        }
    }

    override func numberOfSections(in tableView: UITableView) -> Int {
        return 4
    }

    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        switch section {
        case TO_BUY_HEADING: return 1
        case TO_BUY: return toBuyCollapsed ? 0 : toBuy.getVisibleCount()
        case BOUGHT_HEADING: return 1
        case BOUGHT: return boughtCollapsed ? 0 : bought.getVisibleCount()
        default: return 0
        }
    }
    
    private func swipe(_ indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        let delete: GroceryDragType
        switch indexPath.section {
        case TO_BUY:
            switch toBuy.getItem(indexPath.row) {
            case .item(let item):
                delete = .item(item)
                break
            case .group(let group):
                delete = .group(group)
                break
            default: return nil
            }
            break
        case BOUGHT:
            switch bought.getItem(indexPath.row) {
            case .item(let item):
                delete = .item(item)
                break
            default: return nil
            }
            break
        default: return nil
        }
        let action = UIContextualAction(style: .destructive, title: "Delete") { (action, view, completionHandler) in
            switch delete {
            case .item(let item):
                self.deleteItem(item.id)
                break
            case .group(let group):
                self.deleteGroup(group.id)
                break
            }
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
        case TO_BUY_HEADING:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! LabelButton
            let imageName = toBuyCollapsed ? "chevron.forward.circle.fill" : "chevron.down.circle.fill"
            cell.label.text = "To buy (\(toBuy.getAllItemCount()))"
            cell.button.setImage(UIImage(systemName: imageName), for: .normal)
            return cell
        case TO_BUY:
            switch toBuy.getItem(indexPath.row) {
            case .item(let item):
                let cell = tableView.dequeueReusableCell(withIdentifier: "toBuyListItem") as! LabelButton
                cell.label.text = item.item.render()
                cell.button.tag = indexPath.row
                cell.button.addTarget(self, action: #selector(didTapToBuy), for: .touchUpInside)
                return cell
            case .group(let group):
                let cell = tableView.dequeueReusableCell(withIdentifier: "groupHeader") as! LabelButton
                let imageName = toBuyGroupCollapsed.isGroupCollapsed(group.id) ? "chevron.forward.circle.fill" : "chevron.down.circle.fill"
                cell.button.setImage(UIImage(systemName: imageName), for: .normal)
                cell.label.text = "\(group.group.name) (\(toBuy.getGroupCount(group.id)))"
                return cell
            case .uncategorized:
                let cell = tableView.dequeueReusableCell(withIdentifier: "groupHeader") as! LabelButton
                let imageName = toBuyGroupCollapsed.isGroupCollapsed(nil) ? "chevron.forward.circle.fill" : "chevron.down.circle.fill"
                cell.button.setImage(UIImage(systemName: imageName), for: .normal)
                cell.label.text = "Uncategorized (\(toBuy.getGroupCount(nil)))"
                return cell
            default: return UITableViewCell()
            }
        case BOUGHT_HEADING:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! LabelButton
            let imageName = boughtCollapsed ? "chevron.forward.circle.fill" : "chevron.down.circle.fill"
            cell.label.text = "Bought (\(bought.getAllItemCount()))"
            cell.button.setImage(UIImage(systemName: imageName), for: .normal)
            return cell
        case BOUGHT:
            switch bought.getItem(indexPath.row) {
            case .item(let item):
                let cell = tableView.dequeueReusableCell(withIdentifier: "boughtListItem") as! LabelButton
                cell.label.text = item.item.render()
                cell.button.tag = indexPath.row
                cell.button.addTarget(self, action: #selector(didTapBought), for: .touchUpInside)
                return cell
            case .group(let group):
                let cell = tableView.dequeueReusableCell(withIdentifier: "boughtGroupHeader") as! LabelButton
                let imageName = boughtGroupCollapsed.isGroupCollapsed(group.id) ? "chevron.forward.circle.fill" : "chevron.down.circle.fill"
                cell.button.setImage(UIImage(systemName: imageName), for: .normal)
                cell.label.text = "\(group.group.name) (\(bought.getGroupCount(group.id)))"
                return cell
            case .uncategorized:
                let cell = tableView.dequeueReusableCell(withIdentifier: "boughtGroupHeader") as! LabelButton
                let imageName = boughtGroupCollapsed.isGroupCollapsed(nil) ? "chevron.forward.circle.fill" : "chevron.down.circle.fill"
                cell.button.setImage(UIImage(systemName: imageName), for: .normal)
                cell.label.text = "Uncategorized (\(bought.getGroupCount(nil)))"
                return cell
            default: return UITableViewCell()
            }
        default: return UITableViewCell()
        }
    }

    func tableView(_ tableView: UITableView, itemsForBeginning session: UIDragSession, at indexPath: IndexPath) -> [UIDragItem] {
        let draggable: GroceryDragType
        switch indexPath.section {
        case TO_BUY:
            if let draggable_ = toBuy.getItem(indexPath.row)?.asDraggable {
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
            sanitizedIndexPath = IndexPath(row: min(indexPath.row, toBuy.getVisibleCount() - 1), section: indexPath.section)
            break
        default: return cancel
        }
        tableView.scrollToRow(at: sanitizedIndexPath, at: .none, animated: true)
        if tableView.hasActiveDrag {
            if toBuy.getItem(sanitizedIndexPath.row)?.isGroup ?? false {
                return UITableViewDropProposal(operation: .move, intent: .insertAtDestinationIndexPath)
            }
            if info.isGroup {
                return cancel
            }
            return UITableViewDropProposal(operation: .move, intent: .automatic)
        }
        return cancel
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
                        guard let existing = self.toBuy.getItem(indexPath.row)?.asItem else { return }
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
                            let (drop, addOrder) = self.toBuy.reordered(source: info.indexPath.row, destination: indexPath.row)
                            switch drop {
                            case .item(let i):
                                newOrder = i.item.order + addOrder
                                newGroup = i.item.group
                                break
                            case .group(let g):
                                newOrder = 0
                                newGroup = g
                                break
                            case .uncategorized:
                                newOrder = 0
                                newGroup = nil
                                break
                            default: return
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
                        let (drop, addOrder) = self.toBuy.reordered(source: info.indexPath.row, destination: indexPath.row)
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
                        default: return
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
        let groupHeadingHeight = 36.0
        switch indexPath.section {
        case TO_BUY:
            switch toBuy.getItem(indexPath.row) {
            case .group(_): return groupHeadingHeight
            case .uncategorized: return groupHeadingHeight
            default: break
            }
        case BOUGHT:
            switch bought.getItem(indexPath.row) {
            case .group(_): return groupHeadingHeight
            case .uncategorized: return groupHeadingHeight
            default: break
            }
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
        var toBuyItems: [GroceryListItem] = []
        var boughtItems: [GroceryListItem] = []
        let groups = Database.selectGroups()
        let items = Database.selectGroceries()
        for group in groups {
            toBuyItems.append(.group(group))
            toBuyItems.append(contentsOf: items.filter({
                $0.item.group?.id == group.id && $0.item.active
            }).map({ .item($0) }))
            boughtItems.append(.group(group))
            boughtItems.append(contentsOf: items.filter({
                $0.item.group?.id == group.id && !$0.item.active
            }).map({ .item($0) }))
        }
        toBuyItems.append(.uncategorized)
        toBuyItems.append(contentsOf: items.filter({
            $0.item.group == nil && $0.item.active
        }).map({ .item($0) }))
        boughtItems.append(.uncategorized)
        boughtItems.append(contentsOf: items.filter({
            $0.item.group == nil && !$0.item.active
        }).map({ .item($0) }))
        toBuy = GroceryListItems(toBuyItems, toBuyGroupCollapsed)
        bought = GroceryListItems(boughtItems, boughtGroupCollapsed)
    }
    
    func reloadData() {
        loadData()
        DispatchQueue.main.async {
            self.tableView.reloadData()
        }
    }

    override func viewDidLoad() {
        super.viewDidLoad()
        loadData()
        tableView.dragDelegate = self
        tableView.dropDelegate = self
        tableView.dragInteractionEnabled = true
    }
}
