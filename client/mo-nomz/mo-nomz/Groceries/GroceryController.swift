//
//  GroceryController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/9/21.
//

import UIKit

class GroceryController: UIViewController {
    @IBOutlet weak var export: UIButton!

    var groceryVc: GroceryListController? = nil
    
    @IBAction func export(_ sender: Any?) {
        let items: [ReadableGroceryItemAggregate] = groceryVc?.toBuy ?? []
        let exportText: String = items.map({ (x: ReadableGroceryItemAggregate) -> String in
            return x.item.render()
        }).joined(separator: "\n")
        let vc = UIActivityViewController(activityItems: [exportText], applicationActivities: nil)
        present(vc, animated: true, completion: nil)
    }
    
    private func loadGroceryItems() {
        let completion = { [weak self] (resp: ListGroceryItemResponse) -> Void in
            let items = resp.items
            self?.groceryVc?.toBuy = items.filter({ $0.item.active })
            self?.groceryVc?.bought = items.filter({ !$0.item.active })
            DispatchQueue.main.async {
                self?.export.isEnabled = !items.isEmpty
                self?.groceryVc?.table.reloadData()
            }
        }
        loadGroceryItems(completion: completion)
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? RecipeAddController, segue.identifier == "addItems" {
            vc.onChange = { () -> Void in
                self.loadGroceryItems()
            }
        }
        if let vc = segue.destination as? GroceryListController, segue.identifier == "embedGroceryItems" {
            groceryVc = vc
            loadGroceryItems()
            vc.onChange = { () -> Void in
                self.loadGroceryItems()
            }
        }
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        loadGroceryItems()
    }
}
