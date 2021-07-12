//
//  RecipeDetailController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 7/5/21.
//

import UIKit

class RecipeDetailController: UIViewController {
    @IBOutlet weak var label: UILabel!
    @IBOutlet weak var star1: UIButton!
    @IBOutlet weak var star2: UIButton!
    @IBOutlet weak var star3: UIButton!
    @IBOutlet weak var star4: UIButton!
    @IBOutlet weak var star5: UIButton!
    @IBOutlet weak var link: UIButton!
    
    var recipe: RecipeWithId? = nil
    var onChange: (() -> Void)? = nil
    var detailVc: RecipeDetailListController? = nil
    
    @IBAction func didTapLink(_ sender: Any?) {
        if let link = recipe?.recipe.link {
            let url = URL(string: link)!
            if #available(iOS 10.0, *) {
                UIApplication.shared.open(url, options: [:], completionHandler: nil)
            } else {
                UIApplication.shared.openURL(url)
            }
        }
    }
    
    private func didTapStar(which: Int) {
        let filledStar = UIImage(systemName: "star.fill")
        let unfilledStar = UIImage(systemName: "star")
        switch which {
        case 0:
            star1.setImage(unfilledStar, for: .normal)
            star2.setImage(unfilledStar, for: .normal)
            star3.setImage(unfilledStar, for: .normal)
            star4.setImage(unfilledStar, for: .normal)
            star5.setImage(unfilledStar, for: .normal)
        case 1:
            star1.setImage(filledStar, for: .normal)
            star2.setImage(unfilledStar, for: .normal)
            star3.setImage(unfilledStar, for: .normal)
            star4.setImage(unfilledStar, for: .normal)
            star5.setImage(unfilledStar, for: .normal)
        case 2:
            star1.setImage(filledStar, for: .normal)
            star2.setImage(filledStar, for: .normal)
            star3.setImage(unfilledStar, for: .normal)
            star4.setImage(unfilledStar, for: .normal)
            star5.setImage(unfilledStar, for: .normal)
        case 3:
            star1.setImage(filledStar, for: .normal)
            star2.setImage(filledStar, for: .normal)
            star3.setImage(filledStar, for: .normal)
            star4.setImage(unfilledStar, for: .normal)
            star5.setImage(unfilledStar, for: .normal)
        case 4:
            star1.setImage(filledStar, for: .normal)
            star2.setImage(filledStar, for: .normal)
            star3.setImage(filledStar, for: .normal)
            star4.setImage(filledStar, for: .normal)
            star5.setImage(unfilledStar, for: .normal)
        default:
            star1.setImage(filledStar, for: .normal)
            star2.setImage(filledStar, for: .normal)
            star3.setImage(filledStar, for: .normal)
            star4.setImage(filledStar, for: .normal)
            star5.setImage(filledStar, for: .normal)
        }
        if let r = recipe, which != r.recipe.rating && which > 0 {
            let completion = { () -> Void in
                self.recipe = RecipeWithId(recipe: ReadableRecipe(name: r.recipe.name, link: r.recipe.link, active: r.recipe.active, rating: which, notes: r.recipe.notes, ingredients: r.recipe.ingredients), id: r.id)
                self.onChange?()
                DispatchQueue.main.async {
                    self.view.reloadInputViews()
                }
            }
            updateRecipe(id: r.id, active: r.recipe.active, rating: which, notes: r.recipe.notes, completion: completion)
        }
    }
    
    @IBAction func didTapStar1(_ sender: Any?) {
        didTapStar(which: 1)
    }
    
    @IBAction func didTapStar2(_ sender: Any?) {
        didTapStar(which: 2)
    }
    
    @IBAction func didTapStar3(_ sender: Any?) {
        didTapStar(which: 3)
    }
    
    @IBAction func didTapStar4(_ sender: Any?) {
        didTapStar(which: 4)
    }
    
    @IBAction func didTapStar5(_ sender: Any?) {
        didTapStar(which: 5)
    }
    
    @IBAction func didTapSave(_ sender: Any?) {
        if let r = recipe {
            let completion = {
                DispatchQueue.main.async {
                    self.dismiss(animated: true, completion: nil)
                }
                self.onChange?()
            }
            updateRecipe(id: r.id, active: r.recipe.active, rating: r.recipe.rating, notes: detailVc?.blob.text ?? r.recipe.notes, completion: completion)
        }
    }
    
    @IBAction func didTapCancel(_ sender: Any?) {
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? RecipeDetailListController, segue.identifier == "embedDetail" {
            detailVc = vc
            vc.ingredients = recipe?.recipe.ingredients ?? []
            vc.notes = recipe?.recipe.notes
        }
    }
    
    override func viewDidLoad() {
        label.text = recipe?.recipe.name
        if recipe?.recipe.link == nil {
            link.removeFromSuperview()
        }
        didTapStar(which: recipe?.recipe.rating ?? 0)
    }
}
