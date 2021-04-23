//
//  GroceryListController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/9/21.
//

import MobileCoreServices
import UIKit

class GroceryListController: UITableViewController, UITableViewDragDelegate, UITableViewDropDelegate {
    @IBOutlet weak var table: UITableView!

    var toBuy: [ReadableGroceryItemAggregate] = []
    var bought: [ReadableGroceryItemAggregate] = []
    var onChange: (() -> Void)? = nil
    var mergeItems: (ReadableGroceryItem, ReadableGroceryItem, [Int])? = nil
    var editItem: ReadableGroceryItemAggregate? = nil
    var collapsed: [Bool] = [false, true]
    
    private func hasData() -> Bool {
        return (toBuy.count + bought.count) > 0
    }
    
    func selectRow(_ row: Int) {
        let item = toBuy[row]
        let newItem = ReadableGroceryItem(name: item.item.name, quantity: item.item.quantity, unit: item.item.unit, active: false)
        mergeGroceryItems(groceryItemIds: item.ids, groceryItem: newItem, completion: onChange)
    }
    
    func deselectRow(_ row: Int) {
        let item = bought[row]
        let newItem = ReadableGroceryItem(name: item.item.name, quantity: item.item.quantity, unit: item.item.unit, active: true)
        mergeGroceryItems(groceryItemIds: item.ids, groceryItem: newItem, completion: onChange)
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
                self.table.reloadData()
            }
            break
        case 2:
            editRow(item: toBuy[indexPath.row])
            break
        case 3:
            collapsed[1] = !collapsed[1]
            DispatchQueue.main.async {
                self.table.reloadData()
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
        let handler = { [weak self] (action: UIAlertAction) -> Void in self?.deleteGroceryItems(groceryItemIds: ids, completion: self?.onChange) }
        promptForConfirmation(title: "Delete", message: "Are you sure you want to delete this item?", handler: handler)
    }
    
    func editRow(item: ReadableGroceryItemAggregate) {
        editItem = item
        self.performSegue(withIdentifier: "editItem", sender: nil)
    }
    
    override func tableView(_ tableView: UITableView, trailingSwipeActionsConfigurationForRowAt indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        let ids: [Int]
        switch indexPath.section {
        case 2:
            ids = toBuy[indexPath.row].ids
            break
        case 4:
            ids = bought[indexPath.row].ids
            break
        default:
            return nil
        }
        let action = UIContextualAction(style: .destructive, title: "Delete") { [weak self] (action, view, completionHandler) in
            self?.deleteRow(ids)
            completionHandler(true)
        }
        action.backgroundColor = .systemRed
        return UISwipeActionsConfiguration(actions: [action])
    }
    
    override func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        switch indexPath.section {
        case 1: return self.table.sectionHeaderHeight
        case 3: return self.table.sectionHeaderHeight
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
            return cell
        default:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SectionHeader
            return cell
        }
    }
    
    func tableView(_ tableView: UITableView, itemsForBeginning session: UIDragSession, at indexPath: IndexPath) -> [UIDragItem] {
        var item: ReadableGroceryItemAggregate? = nil
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
                table.scrollToRow(at: indexPath, at: .none, animated: true)
            } else {
                table.scrollToRow(at: IndexPath(row: toBuy.count - 1, section: indexPath.section), at: .none, animated: true)
            }
            break
        case 4:
            if indexPath.row < bought.count {
                table.scrollToRow(at: indexPath, at: .none, animated: true)
            } else {
                table.scrollToRow(at: IndexPath(row: bought.count - 1, section: indexPath.section), at: .none, animated: true)
            }
            break
        default: break
        }
        if table.hasActiveDrag {
            proposal = UITableViewDropProposal(operation: .move, intent: .insertIntoDestinationIndexPath)
        }
        return proposal
    }

    func tableView(_ tableView: UITableView, performDropWith coordinator: UITableViewDropCoordinator) {
        guard let indexPath = coordinator.destinationIndexPath else { return }
        let existing: ReadableGroceryItemAggregate
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
                    let new = try JSONDecoder().decode(ReadableGroceryItemAggregate.self, from: string.data(using: .utf8)!)
                    self.mergeItems = (existing.item, new.item, existing.ids + new.ids)
                    self.performSegue(withIdentifier: "mergeItems", sender: nil)
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
        table.dragDelegate = self
        table.dropDelegate = self
        table.dragInteractionEnabled = true
        table.layer.cornerRadius = 5
    }
}