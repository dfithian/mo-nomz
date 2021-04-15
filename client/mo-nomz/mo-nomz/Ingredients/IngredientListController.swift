//
//  IngredientListController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/9/21.
//

import MobileCoreServices
import UIKit

struct IngredientWithStartingIndex: Codable, Comparable, Equatable {
    let ingredient: ReadableIngredientAggregate
    let startingIndex: Int
    
    static func == (lhs: IngredientWithStartingIndex, rhs: IngredientWithStartingIndex) -> Bool {
        return lhs.startingIndex == rhs.startingIndex
    }

    static func < (lhs: IngredientWithStartingIndex, rhs: IngredientWithStartingIndex) -> Bool {
        return lhs.startingIndex < rhs.startingIndex
    }
}

class IngredientListController: UITableViewController, UITableViewDragDelegate, UITableViewDropDelegate {
    @IBOutlet weak var table: UITableView!

    var ingredients: [IngredientWithStartingIndex] = []
    var bought: [IngredientWithStartingIndex] = []
    var onChange: (() -> Void)? = nil
    var mergeIngredients: (ReadableIngredient, ReadableIngredient, [Int])? = nil
    var editIngredient: ReadableIngredientAggregate? = nil
    var collapsed: Dictionary<Int, Bool> = [1: false, 3: true]
    
    func sortAndReload() {
        ingredients.sort()
        bought.sort()
        DispatchQueue.main.async {
            self.table.reloadData()
        }
    }
    
    func selectRow(row: Int) {
        bought.append(ingredients[row])
        ingredients.remove(at: row)
        sortAndReload()
    }
    
    func deselectRow(row: Int) {
        ingredients.append(bought[row])
        bought.remove(at: row)
        sortAndReload()
    }
    
    override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        switch indexPath.section {
        case 0:
            collapsed[1] = !collapsed[1]!
            DispatchQueue.main.async {
                self.table.reloadData()
            }
        case 1:
            selectRow(row: indexPath.row)
            break;
        case 2:
            collapsed[3] = !collapsed[3]!
            DispatchQueue.main.async {
                self.table.reloadData()
            }
        case 3:
            deselectRow(row: indexPath.row)
            break;
        default:
            break;
        }
    }
    
    override func numberOfSections(in tableView: UITableView) -> Int {
        return 4
    }

    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        if collapsed[section] ?? false { return 0 }
        switch section {
        case 1: return ingredients.count
        case 3: return bought.count
        default: return 1
        }
    }
    
    func deleteRow(ids: [Int]) {
        let ok = UIAlertAction(title: "OK", style: .default, handler: { [weak self] (action) -> Void in Actions.deleteIngredients(ingredientIds: ids, completion: self?.onChange, onError: self?.defaultOnError) })
        let cancel = UIAlertAction(title: "Cancel", style: .cancel, handler: nil)
        let confirmation = UIAlertController(title: "Delete", message: "Are you sure you want to delete this ingredient?", preferredStyle: .alert)
        confirmation.addAction(ok)
        confirmation.addAction(cancel)
        self.present(confirmation, animated: true, completion: nil)
    }
    
    func editRow(ingredient: ReadableIngredientAggregate) {
        editIngredient = ingredient
        self.performSegue(withIdentifier: "editIngredient", sender: nil)
    }
    
    override func tableView(_ tableView: UITableView, trailingSwipeActionsConfigurationForRowAt indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        let ids: [Int]
        switch indexPath.section {
        case 1:
            ids = ingredients[indexPath.row].ingredient.ids
            break
        case 3:
            ids = bought[indexPath.row].ingredient.ids
            break
        default:
            return nil
        }
        let action = UIContextualAction(style: .destructive, title: "Delete") { [weak self] (action, view, completionHandler) in
            self?.deleteRow(ids: ids)
            completionHandler(true)
        }
        action.backgroundColor = .systemRed
        return UISwipeActionsConfiguration(actions: [action])
    }
    
    override func tableView(_ tableView: UITableView, leadingSwipeActionsConfigurationForRowAt indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        let ingredient: ReadableIngredientAggregate
        switch indexPath.section {
        case 1:
            ingredient = ingredients[indexPath.row].ingredient
            break
        case 3:
            ingredient = bought[indexPath.row].ingredient
            break
        default:
            return nil
        }
        let action = UIContextualAction(style: .normal, title: "Edit") { [weak self] (action, view, completionHandler) in
            self?.editRow(ingredient: ingredient)
            completionHandler(true)
        }
        action.backgroundColor = .systemBlue
        return UISwipeActionsConfiguration(actions: [action])
    }
    
    @objc func selectItem(_ sender: Any?) {
        let b = sender as! UIButton
        selectRow(row: b.tag)
    }
    
    @objc func deselectItem(_ sender: Any?) {
        let b = sender as! UIButton
        deselectRow(row: b.tag)
    }
    
    override func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        switch indexPath.section {
        case 0: return self.table.sectionHeaderHeight
        case 2: return self.table.sectionHeaderHeight
        default: return UITableView.automaticDimension
        }
    }

    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        switch indexPath.section {
        case 0:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SectionHeader
            let image = collapsed[1] ?? false ? UIImage(systemName: "chevron.forward.circle.fill") : UIImage(systemName: "chevron.down.circle.fill")
            cell.indicator.setImage(image, for: .normal)
            cell.label.text = "To buy (\(ingredients.count))"
            return cell
        case 1:
            let cell = tableView.dequeueReusableCell(withIdentifier: "ingredientListItem") as! IngredientListItem
            let ingredient = ingredients[indexPath.row].ingredient.ingredient
            cell.name.text = "\(ingredient.quantity.render()) \(ingredient.unit) \(ingredient.name)"
            cell.select.tag = indexPath.row
            cell.select.addTarget(self, action: #selector(selectItem), for: .touchUpInside)
            return cell
        case 2:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SectionHeader
            let image = collapsed[3] ?? false ? UIImage(systemName: "chevron.forward.circle.fill") : UIImage(systemName: "chevron.down.circle.fill")
            cell.indicator.setImage(image, for: .normal)
            cell.label.text = "Bought (\(bought.count))"
            return cell
        case 3:
            let cell = tableView.dequeueReusableCell(withIdentifier: "boughtListItem") as! IngredientListItem
            let ingredient = bought[indexPath.row].ingredient.ingredient
            cell.name.text = "\(ingredient.quantity.render()) \(ingredient.unit) \(ingredient.name)"
            cell.select.tag = indexPath.row
            cell.select.addTarget(self, action: #selector(deselectItem), for: .touchUpInside)
            return cell
        default:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SectionHeader
            return cell
        }
    }
    
    func tableView(_ tableView: UITableView, itemsForBeginning session: UIDragSession, at indexPath: IndexPath) -> [UIDragItem] {
        var ingredient: ReadableIngredientAggregate? = nil
        switch indexPath.section {
        case 1:
            ingredient = ingredients[indexPath.row].ingredient
            break
        case 3:
            ingredient = bought[indexPath.row].ingredient
            break
        default:
            break
        }
        do {
            if let i = ingredient {
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
        guard destinationIndexPath?.section == 1 || destinationIndexPath?.section == 3 else { return proposal }
        guard session.items.count == 1 else { return proposal }
        if table.hasActiveDrag {
            proposal = UITableViewDropProposal(operation: .move, intent: .insertIntoDestinationIndexPath)
        }
        return proposal
    }

    func tableView(_ tableView: UITableView, performDropWith coordinator: UITableViewDropCoordinator) {
        guard let indexPath = coordinator.destinationIndexPath else { return }
        let existing: ReadableIngredientAggregate
        switch indexPath.section {
        case 1:
            existing = ingredients[indexPath.row].ingredient
            break
        case 3:
            existing = bought[indexPath.row].ingredient
            break
        default:
            return
        }
        coordinator.session.loadObjects(ofClass: NSString.self, completion: { items in
            guard let strings = items as? [String] else { return }
            for string in strings {
                do {
                    let new = try JSONDecoder().decode(ReadableIngredientAggregate.self, from: string.data(using: .utf8)!)
                    self.mergeIngredients = (existing.ingredient, new.ingredient, existing.ids + new.ids)
                    self.performSegue(withIdentifier: "mergeIngredients", sender: nil)
                } catch {
                    print("Failed completing drag and drop \(error)")
                }
            }
        })
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? IngredientMergeController, segue.identifier == "mergeIngredients" {
            vc.existing = self.mergeIngredients!.0
            vc.new = self.mergeIngredients!.1
            vc.ingredientIds = self.mergeIngredients!.2
            vc.onChange = onChange
        }
        if let vc = segue.destination as? IngredientEditController, segue.identifier == "editIngredient" {
            vc.existing = self.editIngredient!
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
