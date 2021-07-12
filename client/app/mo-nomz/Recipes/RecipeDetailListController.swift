//
//  RecipeDetailListController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 7/11/21.
//

import UIKit

class RecipeDetailListController: UITableViewController {
    var ingredients: [ReadableIngredient] = []
    var notes: String? = nil
    var beforeHeight: CGFloat? = nil
    var blob: UITextView! = nil

    override func numberOfSections(in tableView: UITableView) -> Int {
        return 4
    }

    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        switch section {
        case 0: return 1
        case 1: return 1
        case 2: return 1
        default: return ingredients.count
        }
    }

    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        switch indexPath.section {
        case 0:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SimpleSectionHeader
            cell.label.text = "Notes"
            return cell
        case 1:
            let cell = tableView.dequeueReusableCell(withIdentifier: "noteItem") as! NoteItem
            cell.blob.text = notes ?? ""
            cell.blob.addDoneButtonOnKeyboard()
            cell.blob.layer.cornerRadius = 10
            blob = cell.blob
            return cell
        case 2:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SimpleSectionHeader
            cell.label.text = "Ingredients"
            return cell
        default:
            let cell = tableView.dequeueReusableCell(withIdentifier: "listItem") as! IngredientListItem
            let item = ingredients[indexPath.row]
            cell.name.text = item.render()
            return cell
        }
    }
}
