//
//  GroceryAdddedItemsController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/23/21.
//

import UIKit

class GroceryAddedItemsController: UITableViewController {
    var items: [ImportGrocerySingle] = []
    
    @objc func didTapDelete(_ sender: Any?) {
        let b = sender as! UIButton
        items.remove(at: b.tag)
        DispatchQueue.main.async {
            self.tableView.reloadData()
        }
    }
    
    override func numberOfSections(in tableView: UITableView) -> Int {
        return 1
    }
    
    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return items.count
    }
    
    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "groceryListItem") as! GroceryAddItem
        cell.name.text = items[indexPath.row].render()
        cell.delete.tag = indexPath.row
        cell.delete.addTarget(self, action: #selector(didTapDelete), for: .touchUpInside)
        return cell
    }
}
