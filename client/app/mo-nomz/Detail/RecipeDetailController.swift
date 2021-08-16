//
//  RecipeDetailController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 7/5/21.
//

import UIKit

class RecipeDetailController: UIViewController, UITextViewDelegate {
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
        guard let r = recipe else { return }
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
        if which != r.recipe.rating && which > 0 {
            let completion = { () -> Void in
                self.loadData()
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
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        guard let r = recipe else { return }
        if let vc = segue.destination as? RecipeDetailListController, segue.identifier == "embedDetail" {
            detailVc = vc
            loadData()
            vc.recipe = r
            vc.ingredients = r.recipe.ingredientsV2.map({ IngredientWithId(id: $0, ingredient: $1) }).sorted(by: { $0.ingredient.order < $1.ingredient.order })
            vc.onChange = { () -> Void in
                self.loadData()
                self.onChange?()
            }
        }
    }
    
    private func loadData() {
        guard let r = recipe else { return }
        let completion = { [weak self] (resp: ReadableRecipe) -> Void in
            self?.detailVc?.recipe = RecipeWithId(recipe: resp, id: r.id)
            self?.detailVc?.ingredients = resp.ingredientsV2.map({ IngredientWithId(id: $0, ingredient: $1) }).sorted(by: { $0.ingredient.order < $1.ingredient.order })
            DispatchQueue.main.async {
                self?.detailVc?.tableView.reloadData()
            }
        }
        getRecipe(id: r.id, completion: completion)
    }
    
    override func viewDidLoad() {
        label.text = recipe?.recipe.name
        if recipe?.recipe.link == nil {
            link.removeFromSuperview()
        }
        didTapStar(which: recipe?.recipe.rating ?? 0)
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        loadData()
    }
}
