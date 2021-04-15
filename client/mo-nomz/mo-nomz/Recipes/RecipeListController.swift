//
//  RecipeListController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/13/21.
//

import MobileCoreServices
import UIKit

struct RecipeWithId: Codable {
    let recipe: ReadableRecipe
    let id: Int
}

class RecipeListController: UITableViewController, UITableViewDragDelegate, UITableViewDropDelegate {
    @IBOutlet weak var table: UITableView!
    
    var active: [RecipeWithId] = []
    var saved: [RecipeWithId] = []
    var onChange: (() -> Void)?
    var editRecipe: RecipeWithId? = nil
    var collapsed: Dictionary<Int, Bool> = [1: false, 3: true]

    override func numberOfSections(in tableView: UITableView) -> Int {
        return 4
    }
    
    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        switch section {
        case 1: return active.count
        case 3: return saved.count
        default: return 1
        }
    }
    
    func openLink(link: String) {
        let url = URL(string: link)!
        if #available(iOS 10.0, *) {
            UIApplication.shared.open(url, options: [:], completionHandler: nil)
        } else {
            UIApplication.shared.openURL(url)
        }
    }
    
    override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        switch indexPath.section {
        case 0:
            collapsed[1] = !collapsed[1]!
            DispatchQueue.main.async {
                self.table.reloadData()
            }
        case 1:
            if let link = active[indexPath.row].recipe.link {
                openLink(link: link)
            }
            break;
        case 2:
            collapsed[3] = !collapsed[3]!
            DispatchQueue.main.async {
                self.table.reloadData()
            }
        case 3:
            if let link = active[indexPath.row].recipe.link {
                openLink(link: link)
            }
            break;
        default:
            break;
        }
    }
    
    func deleteRow(id: Int) {
        let ok = UIAlertAction(title: "OK", style: .default, handler: { [weak self] (action) -> Void in Actions.deleteRecipes(recipeIds: [id], completion: self?.onChange, onError: self?.defaultOnError) })
        let cancel = UIAlertAction(title: "Cancel", style: .cancel, handler: nil)
        let confirmation = UIAlertController(title: "Delete", message: "Are you sure you want to delete this recipe?", preferredStyle: .alert)
        confirmation.addAction(ok)
        confirmation.addAction(cancel)
        self.present(confirmation, animated: true, completion: nil)
    }
    
    func editRow(recipe: RecipeWithId) {
        editRecipe = recipe
        self.performSegue(withIdentifier: "editRecipe", sender: nil)
    }
    
    override func tableView(_ tableView: UITableView, trailingSwipeActionsConfigurationForRowAt indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        let id: Int
        switch indexPath.section {
        case 1:
            id = active[indexPath.row].id
            break
        case 3:
            id = saved[indexPath.row].id
            break
        default:
            return nil
        }
        let action = UIContextualAction(style: .destructive, title: "Delete") { [weak self] (action, view, completionHandler) in
            self?.deleteRow(id: id)
            completionHandler(true)
        }
        action.backgroundColor = .systemRed
        return UISwipeActionsConfiguration(actions: [action])
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
            cell.label.text = "Active (\(active.count))"
            return cell
        case 1:
            let cell = tableView.dequeueReusableCell(withIdentifier: "recipeListItem") as! RecipeListItem
            let name = active[indexPath.row].recipe.name
            cell.name.text = name
            return cell
        case 2:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SectionHeader
            let image = collapsed[3] ?? false ? UIImage(systemName: "chevron.forward.circle.fill") : UIImage(systemName: "chevron.down.circle.fill")
            cell.indicator.setImage(image, for: .normal)
            cell.label.text = "Saved for later (\(saved.count))"
            return cell
        case 3:
            let cell = tableView.dequeueReusableCell(withIdentifier: "recipeListItem") as! RecipeListItem
            let name = saved[indexPath.row].recipe.name
            cell.name.text = name
            return cell
        default:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SectionHeader
            return cell
        }
    }
    
    func tableView(_ tableView: UITableView, itemsForBeginning session: UIDragSession, at indexPath: IndexPath) -> [UIDragItem] {
        var recipe: RecipeWithId? = nil
        switch indexPath.section {
        case 1:
            recipe = active[indexPath.row]
            break
        case 3:
            recipe = saved[indexPath.row]
            break
        default:
            break
        }
        do {
            if let r = recipe {
                let data = try JSONEncoder().encode(r)
                return [UIDragItem(itemProvider: NSItemProvider(item: data as NSData, typeIdentifier: kUTTypePlainText as String))]
            }
        } catch {
            print("Failed to initiate drag and drop \(error)")
        }
        return []
    }
    
    func tableView(_ tableView: UITableView, dropSessionDidUpdate session: UIDropSession, withDestinationIndexPath destinationIndexPath: IndexPath?) -> UITableViewDropProposal {
        var proposal = UITableViewDropProposal(operation: .cancel)
        guard destinationIndexPath != nil else { return proposal }
        guard session.items.count == 1 else { return proposal }
        if table.hasActiveDrag {
            proposal = UITableViewDropProposal(operation: .move, intent: .insertAtDestinationIndexPath)
        }
        return proposal
    }
    
    func tableView(_ tableView: UITableView, performDropWith coordinator: UITableViewDropCoordinator) {
        guard let indexPath = coordinator.destinationIndexPath else { return }
        let willBeActive = [0, 1].contains(indexPath.section)
        coordinator.session.loadObjects(ofClass: NSString.self, completion: { items in
            guard let strings = items as? [String] else { return }
            for string in strings {
                do {
                    let new = try JSONDecoder().decode(RecipeWithId.self, from: string.data(using: .utf8)!)
                    if new.recipe.active != willBeActive {
                        Actions.updateRecipe(id: new.id, active: willBeActive, completion: self.onChange, onError: self.defaultOnError)
                    }
                } catch {
                    print("Failed completing drag and drop \(error)")
                }
            }
        })
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        table.dragDelegate = self
        table.dropDelegate = self
        table.dragInteractionEnabled = true
        table.layer.cornerRadius = 5
    }
}
